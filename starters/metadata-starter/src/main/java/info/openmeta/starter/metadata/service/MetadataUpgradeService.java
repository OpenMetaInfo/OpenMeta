package info.openmeta.starter.metadata.service;

import info.openmeta.framework.web.dto.MetadataUpgradePackage;

import java.util.List;

/**
 * Metadata Upgrade Service.
 */
public interface MetadataUpgradeService {

    /**
     /**
     * Upgrades the metadata of multiple models, all within a single transaction
     * to avoid refreshing the model pool repeatedly and missing dependency data.
     *
     * @param metadataPackages the metadata packages to Upgrade
     */
    void upgradeMetadata(List<MetadataUpgradePackage> metadataPackages);

}
