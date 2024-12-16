package info.openmeta.starter.designer.dto;

import info.openmeta.framework.base.enums.AccessType;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * DTO for row change information
 */
@Data
@NoArgsConstructor
public class RowChangeDTO {

    private String model;
    private Serializable rowId;
    private AccessType accessType;

    // Field values before the change
    private Map<String, Object> dataBeforeChange = new HashMap<>();
    // Field values after the change
    private Map<String, Object> dataAfterChange = new HashMap<>();
    // Current values, stores the latest original values for CREATE and UPDATE operations
    private Map<String, Object> currentData = new HashMap<>();

    private Long lastChangedId;
    private String lastChangedBy;
    private String lastChangedTime;

    public RowChangeDTO(String model, Serializable rowId) {
        this.model = model;
        this.rowId = rowId;
    }

    public void mergeDataBeforeChange(Map<String, Object> newValue) {
        if (newValue != null) {
            this.dataBeforeChange.putAll(newValue);
        }
    }

    public void mergeDataAfterChange(Map<String, Object> newValue) {
        if (newValue != null) {
            this.dataAfterChange.putAll(newValue);
        }
    }

}