  with input as (select pg_read_file('/code/inputs/day202204.txt') as file),
       lines as (select regexp_split_to_table(file, '\n') as line from input),
       lineComponents as (select regexp_split_to_array(line, ',') as ary from lines),
       rangeComponents as (select regexp_split_to_array(ary[1], '-') as fst,
                                  regexp_split_to_array(ary[2], '-') as snd
                             from lineComponents),
       ranges as (select int4range(fst[1]::int4, fst[2]::int4, '[]') as fst,
                         int4range(snd[1]::int4, snd[2]::int4, '[]') as snd
                    from rangeComponents)
select count(case when r.fst @> r.snd or r.fst <@ r.snd then 1 end) as containments,
       count(case when r.fst && r.snd then 1 end)                   as overlappings
  from ranges r;