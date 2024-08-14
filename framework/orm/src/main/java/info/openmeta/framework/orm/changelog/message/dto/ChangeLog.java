package info.openmeta.framework.orm.changelog.message.dto;

import info.openmeta.framework.base.enums.AccessType;
import lombok.Data;

import java.util.Map;

/**
 * ChangeLog DTO
 */
@Data
public class ChangeLog {

    // ChangeLog uuid
    private String uuid;

    private String traceId;

    private String model;
    private Long rowId;
    private AccessType accessType;

    private Map<String, Object> dataBeforeChange;
    private Map<String, Object> dataAfterChange;

    private Long changedId;
    private String changedBy;
    private String changedTime;

}
