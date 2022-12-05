  with input as (select pg_read_file('/code/inputs/day202201.txt') as file),
       line_array as (select regexp_split_to_array(file, '\n') as line
                        from input),
       line_array_with_split_points as (select line,
                                               unnest(array_positions(line, '')) to_position
                                          from line_array),
       line_array_with_split_ranges as (select line,
                                               coalesce(lag(to_position, 1) over (), 0) + 1 from_position,
                                               to_position - 1 as                           to_position
                                          from line_array_with_split_points),
       group_arrays as (select line[from_position:(to_position)] as row
                          from line_array_with_split_ranges),
       group_rows as (select row_number() over () group_number,
                             unnest(row)          calories
                        from group_arrays),
       group_sums as (select sum(calories::int4) sum_calories
                        from group_rows
                       group by group_number
                       order by sum_calories desc
                       limit 3)
select max(sum_calories), sum(sum_calories)
  from group_sums;