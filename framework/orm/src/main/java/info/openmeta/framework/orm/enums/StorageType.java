package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Storage type: RDBMS, ES, Doris
 */
@Getter
@AllArgsConstructor
public enum StorageType {
    RDBMS("RDBMS", "Relational Database Management System"),
    ES("ES", "ElasticSearch"),
    DORIS("Doris", "Doris OLAP");

    @JsonValue
    private final String type;
    private final String name;
}
