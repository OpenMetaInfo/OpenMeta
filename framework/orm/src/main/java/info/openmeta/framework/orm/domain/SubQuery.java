package info.openmeta.framework.orm.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Collection;

/**
 * Sub query conditions, used to specify the fields, filters, orders, pageNumber, pageSize of the sub query.
 * Application scenarios of different field types:
 *      1. ManyToOne, OneToOne: specify the field list to get.
 *      2. OneToMany, ManyToMany: specify the fields to get, filters, orders, aggFunctions, topN.
 *      3. Count query: specify the `count = true` to get the count for every group.
 *      For example: get the count of each department's employees.
 *      Note:
 *      If `filters` is specified, the `count` query will be based on the `filters` conditions.
 *      4. TopN query: specify the topN and orders parameters to get the top N data.
 *      For example: set the topN = 10 and orders = ["createTime", "DESC"].
 *      Note:
 *      The topN query is not supported by all databases,
 *      only databases that support the `ROW_NUMBER()` function and `OVER` clause.
 *      Some databases support the `topN` query, such as:
 *          Oracle Database: 10g and later
 * 	        Microsoft SQL Server: 2005 and later
 * 	        PostgreSQL: 8.4 and later
 * 	        MySQL: 8.0 and later
 * 	        IBM DB2: 8.1 and later
 * 	        SQLite: 3.25.0 and later
 *      5. For databases that do not support the topN query, can only use the MAX/MIN aggregation function
 *      to get the `top 1` data. For example:
 *          aggFunctions = ["MAX", "createTime"] or ["MIN", "createTime"].
 *          which means get the latest or earliest data.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SubQuery {

    @Schema(description = "Sub query fields.", example = "[\"id\", \"name\"]")
    private Collection<String> fields;

    private Filters filters;

    private Orders orders;

    @Schema(description = "Only count the sub records, true/false")
    private Boolean count;

    @Schema(description = "Sub query topN number. Default is no limit.", example = "3")
    private Integer topN;

    public SubQuery(Collection<String> fields) {
        this.fields = fields;
    }

}
