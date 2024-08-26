package info.openmeta.framework.orm.jdbc.database.dialect;

import info.openmeta.framework.base.enums.Operator;

public interface DialectInterface {

    /**
     * Get the predicate of the database query operator: >, =, IN, etc.
     *
     * @param operator FilterUnit operator
     * @return SQL operator predicate string
     */
    String getPredicate(Operator operator);

    /**
     * Get the database paging clause.
     *
     * @param offset offset
     * @param limit limit
     * @return paging clause
     */
    StringBuilder getPageClause(int offset, int limit);
}
