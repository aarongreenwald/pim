/*
 It would be nice if sqlite supported TVFs. Instead I'll have to inline this queries in the code :-(
 */

/*
 Get all subcategories of a category. optionally limit to a level.
 Not very useful for payment queries unless I want a scalar sum or completely ungrouped,
 because if I choose <= 1, for example, I won't include lower level payments, and
 if I don't limit the level, the grouping will not really be right.
 */
WITH RECURSIVE all_subcategories(category_id, name, level, parent) AS (
    select category_id, name, 0 as level, parent_category_id from category where category_id = 17 --rootcategory for the query, would be good to eliminate the need for a base category?
    UNION ALL
    SELECT category.category_id, category.name, level + 1, parent_category_id
    FROM category inner join all_subcategories on parent_category_id = all_subcategories.category_id)
select * from all_subcategories


;
/*
 Rollup all categories to one level beneath a given category, so that I can join payment to this table
 and group payments for a specific category.
 To rollup to the very top, the anchor should be where parent_category_id is null and the case should be
 where level >= 0 (ie removed)
 */
WITH RECURSIVE rollup_category(category_id, group_category_id, level) AS (
    select category_id, category_id, 0 as level
    from category
    where category_id = 17 --set to the root level,
    UNION ALL
    SELECT category.category_id,
           --this case makes sure the first level doesn't get rolled up so there's still
           --the root category and one level of breakout. if I didn't want the root inside the breakout
           --I could eliminate this and pass the parent of the level I want
           case when level >= 1 then rollup_category.group_category_id else category.category_id end,
           level + 1
    from category
             inner join rollup_category on parent_category_id = rollup_category.category_id
)
select rc.*, c.name, gc.name
from rollup_category rc inner join category c on rc.category_id = c.category_id
inner join category gc on rc.group_category_id = gc.category_id
order by rc.category_id;

/*
 A view for a seeing categories inside their parents, for a UI
 */
WITH RECURSIVE all_categories(category_id, name, level, parent) AS (
    select category_id, name, 0 as level, parent_category_id from category where parent_category_id is null
    UNION ALL
    SELECT category.category_id,  replace(hex(zeroblob(level + 1)), '00', '--') || category.name, level + 1, parent_category_id
    FROM category inner join all_categories on parent_category_id = all_categories.category_id
    order by category_id, parent_category_id, level, name
    )
select * from all_categories
