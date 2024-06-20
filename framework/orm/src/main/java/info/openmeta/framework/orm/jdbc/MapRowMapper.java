package info.openmeta.framework.orm.jdbc;

import info.openmeta.framework.base.utils.StringTools;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.support.JdbcUtils;

import java.sql.Date;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * JDBCTemplate Map structure row data encapsulation.
 * To convert the underlined-separated database column key to camel case field name. e.g. dept_id -> deptId.
 */
public class MapRowMapper implements RowMapper<Map<String, Object>> {

    /**
     * @param rs the ResultSet to map (pre-initialized for the current row)
     * @param rowNum the number of the current row
     * @return LinkedHashMap
     */
    @Override
    public Map<String, Object> mapRow(ResultSet rs, int rowNum) throws SQLException {
        ResultSetMetaData metaData = rs.getMetaData();
        int columnCount = metaData.getColumnCount();
        Map<String, Object> resultMap = new LinkedHashMap<>(columnCount);
        for (int i = 1; i <= columnCount; i++) {
            String columnName = JdbcUtils.lookupColumnName(metaData, i);
            String camelCaseName = StringTools.toCamelCase(columnName);
            Object value = JdbcUtils.getResultSetValue(rs, i);
            if (value instanceof java.sql.Date) {
                // Auto-convert java.sql.Date to LocalDate
                value = ((Date) value).toLocalDate();
            }
            resultMap.put(camelCaseName, value);
        }
        return resultMap;
    }
}
