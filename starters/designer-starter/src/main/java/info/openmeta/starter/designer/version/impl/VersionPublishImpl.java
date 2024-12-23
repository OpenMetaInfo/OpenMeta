package info.openmeta.starter.designer.version.impl;

import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.MapUtils;
import info.openmeta.framework.web.dto.MetadataUpgradePackage;
import info.openmeta.starter.designer.dto.RowChangeDTO;
import info.openmeta.starter.designer.dto.ModelChangesDTO;
import info.openmeta.starter.designer.entity.DesignAppEnv;
import info.openmeta.starter.designer.entity.DesignAppVersion;
import info.openmeta.starter.designer.entity.DesignAppVersionPublished;
import info.openmeta.starter.designer.enums.PublishStatus;
import info.openmeta.starter.designer.service.DesignAppVersionPublishedService;
import info.openmeta.starter.designer.upgrade.RemoteApiClient;
import info.openmeta.starter.designer.version.VersionPublish;
import info.openmeta.starter.metadata.constant.MetadataConstant;
import info.openmeta.starter.metadata.service.MetadataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;

/**
 * VersionPublish implementation
 */
@Component
public class VersionPublishImpl implements VersionPublish {

    @Value("${system.name}")
    private String systemName;

    @Autowired
    private ModelService<Long> modelService;

    @Autowired
    private MetadataService metadataService;

    @Autowired
    private RemoteApiClient remoteApiClient;

    @Autowired
    private DesignAppVersionPublishedService designAppVersionPublishedService;

    @Autowired
    private Environment environment;

    /**
     * Convert ModelChangesDTO to MetadataUpgradePackage corresponding to the runtime model, which can be directly published.
     *
     * @param modelChangesDTOList List of design-time changed model data
     * @return List of runtime model changed data
     */
    private List<MetadataUpgradePackage> convertDTOToModelPackage(List<ModelChangesDTO> modelChangesDTOList) {
        List<MetadataUpgradePackage> upgradeModelPackages = new ArrayList<>();
        if (!CollectionUtils.isEmpty(modelChangesDTOList)) {
            for (ModelChangesDTO modelChangesDTO : modelChangesDTOList) {
                MetadataUpgradePackage upgradeModelPackage = new MetadataUpgradePackage();
                String runtimeModel = MetadataConstant.BASIC_METADATA_MODELS.get(modelChangesDTO.getModelName());
                upgradeModelPackage.setModelName(runtimeModel);
                // created data
                List<Map<String, Object>> createRows = modelChangesDTO.getCreatedRows().stream()
                        .map(RowChangeDTO::getCurrentData).toList();
                upgradeModelPackage.setCreateRows(createRows);
                // updated data
                List<Map<String, Object>> updateRows = new ArrayList<>();
                for (RowChangeDTO rowChangeDTO : modelChangesDTO.getUpdatedRows()) {
                    Map<String, Object> rowMap = new HashMap<>();
                    rowMap.put(ID, rowChangeDTO.getRowId());
                    rowMap.putAll(rowChangeDTO.getDataAfterChange());
                    updateRows.add(rowMap);
                }
                upgradeModelPackage.setUpdateRows(updateRows);
                // deleted ids
                List<Serializable> deletedIds = modelChangesDTO.getDeletedRows().stream()
                        .map(RowChangeDTO::getRowId)
                        .toList();
                upgradeModelPackage.setDeleteIds(deletedIds);
                upgradeModelPackages.add(upgradeModelPackage);
            }
        }
        return upgradeModelPackages;
    }

    /**
     * Manually execute the upgrade process through version management
     *
     * @param appEnv App environment to be upgraded
     * @param appVersion App version to be upgraded
     * @param modelChangesDTOList List of changed model data
     */
    @Override
    public void upgradeVersion(DesignAppEnv appEnv, DesignAppVersion appVersion, List<ModelChangesDTO> modelChangesDTOList) {
        // Create published record
        DesignAppVersionPublished appVersionPublished = this.createPublishHistory(appVersion);
        List<MetadataUpgradePackage> upgradeModelPackages = this.convertDTOToModelPackage(modelChangesDTOList);
        if (Boolean.TRUE.equals(appEnv.getAsyncUpgrade())) {
            // Asynchronous upgrade
            this.asyncUpgrade(appEnv, upgradeModelPackages);
        } else {
            LocalDateTime startTime = LocalDateTime.now();
            // Synchronous upgrade
            this.syncUpgrade(appEnv, upgradeModelPackages);
            // After the synchronous publishing is completed, update the published status
            this.updatePublishStatus(appVersionPublished, startTime);
        }
    }

    /**
     * Create a published record before the upgrade, with the status of `Publishing`.
     * @param appVersion App version
     * @return Version published record object
     */
    private DesignAppVersionPublished createPublishHistory(DesignAppVersion appVersion) {
        DesignAppVersionPublished publishRecord = new DesignAppVersionPublished();
        publishRecord.setAppId(appVersion.getAppId());
        publishRecord.setEnvId(appVersion.getEnvId());
        publishRecord.setVersionId(appVersion.getId());
        publishRecord.setPublishStatus(PublishStatus.PUBLISHING);
        publishRecord.setPublishContent(appVersion.getVersionedContent());
        return designAppVersionPublishedService.createOneAndFetch(publishRecord);
    }

    /**
     * After the publishing is completed, record the publishing duration and update the status.
     *
     * @param appVersionPublished Published record
     * @param startTime Publishing start time
     */
    private void updatePublishStatus(DesignAppVersionPublished appVersionPublished, LocalDateTime startTime) {
        // Update the published status and duration statistics of DesignAppVersionPublished
        Double seconds = Duration.between(startTime, LocalDateTime.now()).toMillis() / 1000.0;
        appVersionPublished.setPublishDuration(seconds);
        appVersionPublished.setPublishStatus(PublishStatus.PUBLISHED);
        designAppVersionPublishedService.updateOne(appVersionPublished);
        // Update the published status of DesignAppVersion
        Map<String, Object> appVersionMap = MapUtils.strObj()
                .put(ModelConstant.ID, appVersionPublished.getVersionId())
                .put(DesignAppVersion::getPublished, true)
                .put(DesignAppVersion::getLastPublishTime, startTime).build();
        modelService.updateOne(DesignAppVersion.class.getSimpleName(), appVersionMap);
        // Update the published time of the AppEnv
        Map<String, Object> appEnvMap = MapUtils.strObj()
                .put(ModelConstant.ID, appVersionPublished.getEnvId())
                .put(DesignAppEnv::getLastPublishTime, startTime).build();
        modelService.updateOne(DesignAppEnv.class.getSimpleName(), appEnvMap);
    }

    /**
     * Automatically execute the upgrade process by monitoring changelog and triggering the process
     * @param appEnv     App environment to be upgraded
     * @param changeRow  Changed data
     */
    public void upgradeAuto(DesignAppEnv appEnv, Map<String, Object> changeRow) {
        // TODO: Automatic upgrade, triggered by changelog monitoring and process triggering
    }

    /**
     * Synchronous upgrade
     * @param appEnv App environment to be upgraded
     * @param modelPackages List of runtime model upgrade data
     */
    private void syncUpgrade(DesignAppEnv appEnv, List<MetadataUpgradePackage> modelPackages) {
        if (Objects.equals(systemName, appEnv.getAppCode())) {
            String envType = appEnv.getEnvType().name().toLowerCase();
            if (Arrays.asList(environment.getActiveProfiles()).contains(envType)) {
                // Upgrade the current service
                metadataService.upgradeMetadata(modelPackages);
                metadataService.reloadMetadata();
                return;
            }
        }
        // Remote environment upgrade
        remoteApiClient.remoteUpgrade(appEnv, modelPackages);
    }

    /**
     * Asynchronous upgrade
     * @param appEnv App environment to be upgraded
     * @param modelPackages List of runtime model upgrade data
     */
    private void asyncUpgrade(DesignAppEnv appEnv, List<MetadataUpgradePackage> modelPackages) {
        // TODO Asynchronous upgrade, push to MQ, the consumer decides to upgrade the current environment or remote environment
        this.syncUpgrade(appEnv, modelPackages);
    }

}