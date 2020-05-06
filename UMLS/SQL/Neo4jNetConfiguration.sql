alter proc Neo4jNetConfiguration @style varchar(10)='', @likeTerm varchar(2000)='%', @inTerm varchar(2000)='', @dir varchar(10)='in-out' as

-- Generate Neo4j instructions to create vertices and edges corresponding to the supplied parameter values
-- Note that ambiguous status values in source revords warrant identification of SNOMED records to be
-- selected as authentic and current concepts and relationships as defined by the SNOMED consortium
-- Terms can be browsed at https://browser.ihtsdotools.org
-- Source records considered authoratative are identified with a 1 in the DukeSelect column of the descriptions and
-- relationships tables (assigned by the SnomedAuthorativeAssignment proc) 

set nocount on

declare @t0 table(dir varchar(10) not null, cID1 varchar(25) not null, cID2 varchar(25) null, dID1 int null, dID2 int null, drID int null)
declare @t1 table(dir varchar(10) not null, cID1 varchar(25) not null, cID2 varchar(25) null, dID1 int null, dID2 int null, drID int null)
declare @done bit=0
declare @inTermList table(term varchar(255))
declare @p1 smallint, @p2 smallint

-- Parse search terms
-- Retain spaces, since some terms contain them
--select @inTerm=replace(@inTerm, ' ', '')
if(left(@inTerm, 1)<>'|')
  select @inTerm='|'+@inTerm
if(right(@inTerm, 1)<>'|')
  select @inTerm=@inTerm+'|'
select @p1=1, @p2=charindex('|', @inTerm, 2)
while(@p2>0)
  begin
    if(@p2-@p1>1)
      insert into @inTermList select substring(@inTerm, @p1+1, @p2-@p1-1)
    select @p1=@p2, @p2=charindex('|', @inTerm, @p2+1)
  end

if(@style in(''))

  begin

    -- Retrieve concepts with terms of interest
    insert into @t0(dir, cID1, cID2, dID1, dID2)
    select @dir, conceptID, conceptID, DukeID, DukeID
    from   description
    where  DukeSelection=1 and (term like @likeTerm or term in(select term from @inTermList))

    -- Retrieve lineage of related concepts in antecedant and descendant directions
    -- Note that paths are followed strictly in a descendant direction for outbound vertices
    -- and in an antecedent direction for inbound vertices
    -- This limits related vertices to those in direct line to those that satisfy initial selection, above
    select @done=0 
    while(@done=0)
      begin
        -- Populate temp table with appropriate in/out edges for identified vertices
        -- Copy them to staging table and repeat
        -- Terminate when no additional edges exist
        delete @t1
        if(@dir like '%out%')
          -- Record outbound edges on outbound paths
          -- Ignore any that are already recorded
          insert into @t1
          select distinct 'out', r.sourceID, r.destinationID, d1.DukeID, d2.DukeID, r.DukeID
          from   @t0 t join relationship r on t.dir like '%out%' and t.cID2=r.sourceID
                 join description d1 on r.sourceID=d1.conceptID
                 join description d2 on r.destinationID=d2.conceptID
                 left join @t0 t2 on r.DukeID=t2.drID
          where  r.DukeSelection=1 and d1.DukeSelection=1 and d2.DukeSelection=1 and t2.cID1 is null
        if(@dir like '%in%')
          -- Record inbound edges on inbound paths
          -- Ignore any that are already recorded
          insert into @t1
          select distinct 'in', r.sourceID, r.destinationID, d1.DukeID, d2.DukeID, r.DukeID
          from   @t0 t join relationship r on t.dir like '%in%' and t.cID1=r.destinationID
                 join description d1 on r.sourceID=d1.conceptID
                 join description d2 on r.destinationID=d2.conceptID
                 left join @t0 t2 on r.DukeID=t2.drID
          where  r.DukeSelection=1 and d1.DukeSelection=1 and d1.DukeSelection=1 and t2.cID1 is null
        -- Record edge points or quit, if all have been recorded
        if(exists(select * from @t1))
          insert into @t0 select dir, cID1, cID2, dID1, dID2, drID from @t1
        else
          select @done=1
      end

    -- Include any edges between identified vertices that do not already appear in edge list
    -- Edges can be omitted due to unidirectional traversal of vertices, as done above
    -- When traversing in reverse (forward), an encountered vertex may have a forward
    -- association with a vertex that is not encountered in a forward (reverse) traversal
    insert into @t0
    select distinct 'out', id1.cID, id2.cID, id1.dID, id2.dID, r.DukeID
    from   (select cID1 as cID, dID1 as dID from @t0 union select cID2, dID2 from @t0) id1
           cross join (select cID1 as cID, dID1 as dID from @t0 union select cID2, dID2 from @t0) id2
           join relationship r on id1.cID=r.sourceID and id2.cID=r.destinationID
           left join @t0 t on r.DukeID=t.drID
           where r.DukeSelection=1 and t.cID1 is null

    -- Return vertex info
    -- Prefix IDs with a character to satisfy Neo4j requirements
    select 'vertex' as rectype, 'c'+t.cID1 as conceptID1, d1.term, d2.term as type, null as conceptID2 --, t.dID1 as DukeID
    from   @t0 t join description d1 on t.dID1=d1.DukeID
           join description d2 on d1.typeID=d2.conceptID
    where  d2.DukeSelection=1
    union
    select 'vertex' as rectype, 'c'+t.cID2 as conceptID1, d1.term, d2.term as type, null as conceptID2 --, t.dID2 as DukeID
    from   @t0 t join description d1 on t.dID2=d1.DukeID
           join description d2 on d1.typeID=d2.conceptID
    where  d2.DukeSelection=1
    -- Return edge info
    -- Note that the default (distinct) union behavior omits dupicate edges
    union
    select 'edge' as rectype, 'c'+t.cID1 as conceptID1, null as term, d.term as type, 'c'+t.cID2 as conceptID2 --, t.drID as DukeID
    from   @t0 t join relationship r on t.drID=r.DukeID
           join description d on r.typeID=d.conceptID
    where  d.DukeSelection=1
    order by rectype desc, conceptID1, conceptID2

  end
