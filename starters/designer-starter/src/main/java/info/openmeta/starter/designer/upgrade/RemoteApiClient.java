package info.openmeta.starter.designer.upgrade;

import info.openmeta.framework.web.dto.MetadataUpgradePackage;
import info.openmeta.starter.designer.entity.DesignAppEnv;

import java.util.List;

public interface RemoteApiClient {

    /**
     * Remote call to upgrade API
     * @param appEnv        App environment
     * @param modelPackages List of runtime model data packages to be upgraded
     */
    void remoteUpgrade(DesignAppEnv appEnv, List<MetadataUpgradePackage> modelPackages);
}