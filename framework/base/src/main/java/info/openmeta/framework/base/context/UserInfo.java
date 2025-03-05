package info.openmeta.framework.base.context;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Basic info of the current user
 */
@Data
public class UserInfo implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;
    private String name;
    private String language;
    private String timezone;
    private Long empId;
    private Long deptId;
    private Long companyId;
    private String tenantId;
    private String datasourceKey;

}
