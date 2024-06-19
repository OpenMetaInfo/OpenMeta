package info.openmeta.framework.base.context;

import lombok.Data;

import java.io.Serializable;
import java.util.Set;

/**
 * Permission info of the current user
 */
@Data
public class UserPermission implements Serializable {

    private Set<Long> roleIds;
    private Set<String> roleCodes;
}
