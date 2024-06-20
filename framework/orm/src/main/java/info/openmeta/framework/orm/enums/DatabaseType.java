package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum DatabaseType {
    MYSQL("MySQL"),
    POSTGRESQL("PostgreSQL"),
    ORACLE("Oracle"),
    SQLSERVER("SQLServer"),
    TIDB("TiDB"),
    ELASTICSEARCH("ElasticSearch"),
    MONGODB("MongoDB"),
    REDIS("Redis");

    @JsonValue
    private final String type;
}
