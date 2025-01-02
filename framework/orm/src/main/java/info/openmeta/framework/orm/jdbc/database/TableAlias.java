package info.openmeta.framework.orm.jdbc.database;

import java.util.HashMap;
import java.util.Map;

/**
 * Table alias management class
 */
public class TableAlias {

    public static final String MAIN_TABLE_ALIAS = "t";
    // the prefix of the table alias of the translation table
    public static final String TRANS_TABLE_ALIAS = "trans";

    /**
     * The associated table alias with the field chain as the key, which may use different fields of the key associated table,
     * but their table aliases are the same table, such as jobId.name, jobId.code;
     * In addition, the associated tables of different field chain keys may be the same table,
     * but are joined through different left join statements, in this case, they are two different table aliases,
     * such as deptId.name, parentDeptId.name
     * For example,
     *      `A left join B on A.f=B.id` only join once when taking multiple fields of B through the field `f`;
     *      `A left join B b1 on A.f1=b1.id left join B b2 on A.f2=b2.id` when A's f1 and f2 fields respectively join B,
     *       it belongs to join multiple times, and table B has different aliases in this case,
     *       so the keys (in chainTableAlias) of B alias is f1, f2 respectively, and the multi-level cascade is the same.
     */
    private final Map<String, String> chainTableAliasMap = new HashMap<>();

    /**
     * Table alias to translation table alias.
     */
    private final Map<String, String> transTableAliasMap = new HashMap<>();

    /**
     * Get the alias of the right table by the field chain.
     * If the field chain has been associated with a table alias, return the table alias directly;
     *
     * @param fieldChain Table alias key, that is, the left part of the cascading field,
     *                   such as `jobId` of `jobId.name`, `jobId.typeId` of `jobId.typeId.name`.
     * @return Alias of the right table
     */
    public String getRightTableAlias(String fieldChain) {
        String rightTableAlias;
        if (chainTableAliasMap.containsKey(fieldChain)) {
            rightTableAlias = chainTableAliasMap.get(fieldChain);
        } else {
            // Generate a new table alias based on the size +1
            rightTableAlias = MAIN_TABLE_ALIAS + (chainTableAliasMap.size() + 1);
            // Update the table alias dictionary
            this.chainTableAliasMap.put(fieldChain, rightTableAlias);
        }
        return rightTableAlias;
    }

    /**
     * Get the translation table alias for the specified table alias
     *
     * @param tableAlias Table alias
     * @return Alias of the translation table
     */
    public String getTransTableAlias(String tableAlias) {
        return transTableAliasMap.get(tableAlias);
    }

    /**
     * Generate the translation table alias for the specified table alias
     *
     * @param tableAlias Table alias
     * @return Alias of the translation table
     */
    public String generateTransTableAlias(String tableAlias) {
        // Generate a new trans table alias based on the size +1
        String transTableAlias = TRANS_TABLE_ALIAS + (transTableAliasMap.size() + 1);
        this.transTableAliasMap.put(tableAlias, transTableAlias);
        return transTableAlias;
    }
}
