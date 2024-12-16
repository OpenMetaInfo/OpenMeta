package info.openmeta.starter.designer.version;

import info.openmeta.starter.designer.dto.ModelChangesDTO;
import info.openmeta.starter.designer.entity.DesignAppEnv;
import info.openmeta.starter.designer.entity.DesignAppVersion;

import java.util.List;
import java.util.Map;

/**
 * Version publish management, including synchronous and asynchronous upgrades
 */
public interface VersionPublish {

    /**
     * Manually execute the upgrade process through version management
     *
     * @param appEnv App environment to be upgraded
     * @param appVersion App version to be upgraded
     * @param modelChangesDTOList List of changed model data
     */
    void upgradeVersion(DesignAppEnv appEnv, DesignAppVersion appVersion, List<ModelChangesDTO> modelChangesDTOList);

    /**
     * Automatically execute the upgrade process by monitoring changelog and triggering the process
     *
     * @param appEnv App environment to be upgraded
     * @param changeRow Changed data
     */
    void upgradeAuto(DesignAppEnv appEnv, Map<String, Object> changeRow);

}