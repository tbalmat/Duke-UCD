USE [Snomed]
GO
/****** Object:  StoredProcedure [dbo].[SnomedAuthorativeAssignment]    Script Date: 5/19/2020 1:17:41 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER proc [dbo].[SnomedAuthorativeAssignment] as

-- Identify authorative source SNOMED records

-- Observations:

-- Typically, multiple description records exist per date, with different caseSignificantId codes
-- Treatment of case is indicated by the concept corresponding to caseSignificantId, but is ignored here

-- Case:
-- Multiple SNOMED terms have been observed that apparently refer to a single concept (Silvered leaf monkey, for
-- instance, with spellings in upper and lower case).  The description table contains a column labeled "Active."
-- Since terms were imported exactly as they appear in  the source data, care must be taken when searching for
-- values in a case sensitive environment.

-- Ambiguity of concept and relationship status:
-- The active columns of the description and relationship tables appear to indicate item obsolescence
-- Active and inactive statii have been observed for the most recent date of a single concept (i.e. Silvered leaf monkey)
-- A review of the SNOMED browser (https://browser.ihtsdotools.org) indicates active concepts for certain of these
-- Therefore, a concept will be considered active if it appears as both active and inactive in the source data

-- Source records considered authoratative are identified with a 1 in the DukeSelect column of the descriptions and
-- relationships tables

-- The following leaves, at most, one synonym record (typeID='900000000000013056') active per concept

-- SNOMED terms and relationships can be browsed at https://browser.ihtsdotools.org

--
-- Descriptions
--

-- Inactivate all concepts
update description set DukeSelection=0

-- Identify, for the most recent date and type of each concept ID, whether an authorative (active=1) record exists
-- Include authorative records in Duke selection
update d set DukeSelection=1
from   description d
       join ( -- Assemble set of Duke IDs for authentication status
              select act.DukeID
              from   ( -- Retrieve most recent date for each concept ID, type pair
                       select   conceptID, typeID, max(effectiveTime) as dt
                       from     description
                       group by conceptID, typeID
                     ) dt
                     join ( -- Retrieve min Duke ID for active records of each concept, type, and date
                            select   conceptID, typeID, effectiveTime as dt, min(DukeID) as DukeID
                            from     description
                            where    active=1
                            group by conceptID, typeID, effectiveTime
                          ) act on dt.conceptID=act.conceptID and dt.typeID=act.typeID and dt.dt=act.dt
            ) auth on d.DukeID=auth.DukeID
-- Select one record for each type (FSN and synonym)
-- Omit the following where clause
--where  -- Omit duplicated concept IDs for types
--       -- As of August 2019, entries in the description table have one of two values in the typeID column:  900000000000002944 or 900000000000013056
--       -- There also exist active description records with:
--       --          conceptID              typeID  term
--       -- 900000000000002944  900000000000002944  Fully specified name (core metadata concept)
--       -- 900000000000002944  900000000000013056  Fully specified name
--       -- 900000000000013056  900000000000002944  Synonym (core metadata concept)
--       -- 900000000000013056  900000000000013056  Synonym
--       -- Since conceptID is used to map typeID from description records and conceptID is confounded (both 2944 records give a similar result,
--       -- as do both 13056 records), omit the record with the most characters in its term
--       d.conceptID not in('900000000000002944', '900000000000013056') or d.typeID='900000000000013056'

--
-- Relationships
--

-- Inactivate all relationships
update relationship set DukeSelection=0

-- Identify, for the most recent date for each source, destination concept ID pair, whether an authorative record exists
-- Include authorative records in Duke selection
-- Ignore relationships for which the most recent record is inactive (apparent method of declaring obsolescence)
-- Examples include:
-- IDs 106941001->54121002 (Family Cercopithecidae -> Monkey) and
--     378961004->4748007 (Kingdom Animalia -> Living organism)
-- Both of these examples relate fully specified names to synonyms, which may be a viloation of semantic rules
-- Order relationships (sourceID->destinationID) by date, type, and status
-- Identify sourceID, destinationID pairs, within type, where each ID is included in Duke selection
-- For each pair, identify the most recent date, within type, and evaluate relationship statii for that date
-- Include one active relationship per pair and type, if one exists

update r set DukeSelection=1
from   relationship r
       join ( -- Assemble set of Duke IDs for authentication status
              select act.DukeID
              from   ( -- Retrieve most recent date for each source, destination, type triplet
                       select   r.sourceID, r.destinationID, r.typeID, max(r.effectiveTime) as dt
                       from     relationship r join description d1 on r.sourceID=d1.conceptID and d1.DukeSelection=1
                                join description d2 on r.destinationID=d2.conceptID and d2.DukeSelection=1
                       group by r.sourceID, r.destinationID, r.typeID
                     ) dt
                     join ( -- Retrieve min Duke ID for active records of each source, destination, type, and date
                            select   sourceID, destinationID, typeID, effectiveTime as dt, min(DukeID) as DukeID
                            from     relationship
                            where    active=1
                            group by sourceID, destinationID, typeID, effectiveTime
                          ) act on dt.sourceID=act.sourceID and dt.destinationID=act.destinationID and dt.typeID=act.typeID and dt.dt=act.dt
            ) auth on r.DukeID=auth.DukeID

-- Omit concepts for confounded relation type IDs (each type ID in use as of August 2019 has two concept records:  one with typeID
-- 900000000000002944 and one with typeID 900000000000013056
-- All such concept records with typeID 900000000000002944 have term identical to corresponding (same conceptID) records with
-- typeID 900000000000013056, except with the text '(attribute)' appended
-- De-select concepts with typeID 900000000000002944 referenced as typeID in relationship records
-- CORRECTION:  typeID 900000000000002944 corresponds to concept 'Fully specified name' while typeID corresponds to
-- 'Synonym' [review with select * from description where conceptid in(900000000000013056, 900000000000002944)]
-- Instead of disabling FSN types, leave enabled and take care to include typeIDs in join clauses
-- For instance, to join FSNs with a relationship use:
-- select fsn1.term, fsn2.term
-- from   description fsn1 join relationship r on fsn1.conceptid=r.sourceid
--        join description rd on r.typeid=rd.conceptid
--        join description fsn2 on r.destinationid=fsn2.conceptid
-- where  fsn1.conceptid='64572001' and fsn1.typeid='900000000000002944' and fsn1.dukeselection=1
--        and r.dukeselection=1 and rd.typeid='900000000000002944' and rd.dukeselection=1
--        and fsn.typeid='900000000000002944' and fsn.dukeselection=1
-- Note the common specification of FSN type IDs
-- For synonyms, use typeID='900000000000013056' (note that many synonyms may exist for a single FSN)
-- Following update that inactivates FSNs is disabled
--update description set DukeSelection=0
--where  conceptID in(select distinct typeID from relationship) and typeID='900000000000002944' and DukeSelection=1
 