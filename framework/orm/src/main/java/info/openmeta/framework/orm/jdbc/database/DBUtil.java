package info.openmeta.framework.orm.jdbc.database;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.ConfigurationException;
import info.openmeta.framework.orm.enums.DatabaseType;
import info.openmeta.framework.orm.jdbc.database.dialect.DialectInterface;
import info.openmeta.framework.orm.jdbc.database.dialect.MySQLDialect;
import jakarta.annotation.PostConstruct;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Database utility class
 */
@Component
public class DBUtil {

    private static String READ_PRIMARY_SQL_HINT;

    private static DialectInterface dbDialect;

    @Value("${sql.hint.read-primary:!readPrimary=true}")
    private String readPrimarySqlHint;

    @Value("${spring.datasource.url}")
    private String jdbcUrl;

    /**
     * Initialize static constants, such as database dialect and assign values to dbDialect
     */
    @PostConstruct
    public synchronized void initDBType() {
        READ_PRIMARY_SQL_HINT = readPrimarySqlHint;
        if (jdbcUrl != null && jdbcUrl.startsWith("jdbc:")) {
            String withoutJdbc = jdbcUrl.substring(5);
            int colonIndex = withoutJdbc.indexOf(":");
            if (colonIndex > 0) {
                String dbType = withoutJdbc.substring(0, colonIndex);
                DatabaseType databaseType = DatabaseType.valueOf(dbType.toUpperCase());
                if (databaseType == DatabaseType.MYSQL) {
                    dbDialect = new MySQLDialect();
                } else {
                    throw new ConfigurationException("Dialects of database {0} are not currently supported!", databaseType.getType());
                }
                return;
            }
        }
        throw new ConfigurationException("The 'spring.datasource.url' configuration is invalid because of missing database type!");
    }

    /**
     * Convert Operator to SQL query condition predicate
     * @param operator Operator
     * @return SQL query condition predicate
     */
    public static String getPredicate(Operator operator) {
        return dbDialect.getPredicate(operator);
    }

    /**
     * Get the paging clause according to the page number and page size.
     * Calculate the offset based on the page number and page size.
     *
     * @param pageNumber Page number starts from 1, corresponding to the offset starting from 0
     * @param pageSize Page size is controlled between [0, MAX_BATCH_SIZE]
     * @return Paging SQL clause
     */
    public static StringBuilder getDialectPageClause(int pageNumber, int pageSize) {
        int size = pageSize < 1 ? 1 : Math.min(pageSize, BaseConstant.MAX_BATCH_SIZE);
        int offset = pageNumber <= 1 ? 0 : Math.max((pageNumber - 1) * size, 0);
        return dbDialect.getPageClause(offset, size);
    }

    /**
     * Wrap the SQL hint, such as reading the primary database according to the annotation configuration,
     * and add the SQL hint in front of the original SQL statement.
     *
     * @param originSql Original SQL
     * @return Wrapped SQL
     */
    public static String wrapHint(String originSql) {
        Context context = ContextHolder.getContext();
        if (context.isReadPrimary()) {
            // Add the SQL Hint for reading the primary database
            String sqlHint = "/* " + READ_PRIMARY_SQL_HINT + " */ ";
            return sqlHint + originSql;
        }
        return originSql;
    }
}
