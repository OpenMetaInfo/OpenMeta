package info.openmeta.framework.orm.jdbc;

import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.orm.annotation.ExecuteSql;
import info.openmeta.framework.orm.annotation.WriteOperation;
import info.openmeta.framework.orm.jdbc.database.SqlParams;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.stereotype.Repository;

import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * A proxy class for executing JDBC operations, offering parameterized methods,
 * dynamic data source switching (via modelName), and SQL logging.
 */
@Slf4j
@Repository
public class JdbcProxy {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    /**
     * Inserts a new row and returns the auto-generated primary key (of type Long).
     *
     * @param modelName identifies the model; can be used to switch the data source
     * @param sqlParams encapsulates the SQL statement and parameters
     * @return the generated primary key as a Long
     */
    @WriteOperation
    @ExecuteSql
    public Long insert(String modelName, SqlParams sqlParams) {
        KeyHolder keyHolder = new GeneratedKeyHolder();
        jdbcTemplate.update(connection -> {
            PreparedStatement ps = connection.prepareStatement(sqlParams.getSql(), Statement.RETURN_GENERATED_KEYS);
            for (int i = 0; i < sqlParams.getArgs().size(); i++) {
                ps.setObject(i + 1, sqlParams.getArgs().get(i));
            }
            return ps;
        }, keyHolder);
        // Ensure the auto-increment primary key is interpreted as a Long.
        return Cast.of(Objects.requireNonNull(keyHolder.getKey()).longValue());
    }

    /**
     * Executes an update statement and returns the number of affected rows.
     *
     * @param modelName identifies the model; can be used to switch the data source
     * @param sqlParams encapsulates the SQL statement and parameters
     * @return the number of rows affected by the update
     */
    @WriteOperation
    @ExecuteSql
    public int update(String modelName, SqlParams sqlParams) {
        return jdbcTemplate.update(sqlParams.getSql(), sqlParams.getArgsArray());
    }

    /**
     * Performs a batch update for the provided SQL and parameter sets.
     * Returns the total number of rows affected across all batches.
     *
     * @param modelName identifies the model; can be used to switch the data source
     * @param sqlParams encapsulates the SQL statement and parameters
     * @param batchArgs a list of parameter arrays, one for each batch
     * @return the total number of rows affected across all batch executions
     */
    @WriteOperation
    @ExecuteSql
    public int batchUpdate(String modelName, SqlParams sqlParams, List<Object[]> batchArgs) {
        int[] affectedRows = jdbcTemplate.batchUpdate(sqlParams.getSql(), batchArgs);
        return Arrays.stream(affectedRows).sum();
    }

    /**
     * Executes a query and returns the results as a list of maps,
     * with each map's keys in camelCase. This utilizes {@link MapRowMapper}.
     *
     * @param modelName identifies the model; can be used to switch the data source
     * @param sqlParams encapsulates the SQL statement and parameters
     * @return a list of maps representing each row of the result set
     */
    @ExecuteSql
    public List<Map<String, Object>> queryForList(String modelName, SqlParams sqlParams) {
        return jdbcTemplate.query(sqlParams.getSql(), new MapRowMapper(), sqlParams.getArgsArray());
    }

    /**
     * Executes a query that returns a single value of the specified type
     * (e.g., the result of a COUNT(*) operation).
     *
     * @param modelName   identifies the model; can be used to switch the data source
     * @param sqlParams   encapsulates the SQL statement and parameters
     * @param entityClass the class representing the expected return type
     * @return a single value of the specified type, or null if no result is found
     */
    @ExecuteSql
    public Object queryForObject(String modelName, SqlParams sqlParams, Class<?> entityClass) {
        return jdbcTemplate.queryForObject(sqlParams.getSql(), entityClass, sqlParams.getArgsArray());
    }

}