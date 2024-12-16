package info.openmeta.starter.designer.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.designer.dto.ModelChangesDTO;
import info.openmeta.starter.designer.entity.DesignAppEnv;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * DesignAppEnv Model Service Interface
 */
public interface DesignAppEnvService extends EntityService<DesignAppEnv, Long> {

    /**
     * Get all not versioned changes of the specified App env.
     *
     * @param envId App env ID
     * @return List of model changes DTO
     */
    List<ModelChangesDTO> getNotVersionedChanges(Long envId);

    /**
     * Preview the changes between the source and target environments.
     *
     * @param sourceEnvId Source Environment ID
     * @param targetEnvId Target Environment ID
     * @return List of model changes DTO
     */
    List<ModelChangesDTO> previewBetweenEnv(@RequestParam Long sourceEnvId, @RequestParam Long targetEnvId);

    /**
     * Merge the changes between the source and target environments.
     *
     * @param sourceEnvId Source Environment ID
     * @param targetEnvId Target Environment ID
     */
    void mergeBetweenEnv(@RequestParam Long sourceEnvId, @RequestParam Long targetEnvId);

}