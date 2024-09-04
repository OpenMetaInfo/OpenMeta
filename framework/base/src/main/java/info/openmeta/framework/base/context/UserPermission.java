package info.openmeta.framework.base.context;

import lombok.Data;

import java.io.Serializable;
import java.util.Set;

/**
 * Role codes of the current user
 */
@Data
public class UserPermission implements Serializable {
    private Set<String> roleCodes;
}

