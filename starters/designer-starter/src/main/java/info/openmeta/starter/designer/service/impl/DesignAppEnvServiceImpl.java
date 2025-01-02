package info.openmeta.starter.designer.service.impl;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.constant.VersionConstant;
import info.openmeta.starter.designer.dto.ModelChangesDTO;
import info.openmeta.starter.designer.dto.RowChangeDTO;
import info.openmeta.starter.designer.entity.DesignAppEnv;
import info.openmeta.starter.designer.entity.DesignAppEnvMerge;
import info.openmeta.starter.designer.entity.DesignAppVersion;
import info.openmeta.starter.designer.service.DesignAppEnvMergeService;
import info.openmeta.starter.designer.service.DesignAppEnvService;
import info.openmeta.starter.designer.service.DesignAppVersionService;
import info.openmeta.starter.designer.version.VersionControl;
import info.openmeta.starter.metadata.constant.MetadataConstant;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestParam;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * DesignAppEnv Model Service Implementation
 */
@Service
public class DesignAppEnvServiceImpl extends EntityServiceImpl<DesignAppEnv, Long> implements DesignAppEnvService {

    @Autowired
    private VersionControl versionControl;

    @Autowired
    private DesignAppVersionService appVersionService;

    @Autowired
    private DesignAppEnvMergeService envMergeService;

    /**
     * Get all not versioned changes of the specified App env.
     *
     * @param envId App env ID
     * @return List of model changes DTO
     */
    @Override
    public List<ModelChangesDTO> getNotVersionedChanges(Long envId) {
        List<ModelChangesDTO> modelChangesDTOList = new ArrayList<>();
        DesignAppEnv appEnv = this.getById(envId)
                .orElseThrow(() -> new IllegalArgumentException("The specified envId does not exist! {0}", envId));
        LocalDateTime lastVersionedTime = this.getLastVersionedTime(envId);
        for (String versionedModel : MetadataConstant.BASIC_METADATA_MODELS.keySet()) {
            ModelChangesDTO modelChangesDTO = versionControl.getModelChanges(appEnv, versionedModel, lastVersionedTime);
            if (modelChangesDTO != null) {
                modelChangesDTOList.add(modelChangesDTO);
            }
        }
        return modelChangesDTOList;
    }

    /**
     * Get the last versioned time of the specified environment.
     *
     * @param envId Env ID
     * @return Last versioned time
     */
    private LocalDateTime getLastVersionedTime(Long envId) {
        Filters filters = new Filters().eq(DesignAppVersion::getEnvId, envId);
        Orders orders = Orders.ofDesc(DesignAppVersion::getCreatedTime);
        FlexQuery flexQuery = new FlexQuery(filters, orders);
        flexQuery.setLimitSize(1);
        DesignAppVersion appVersion = appVersionService.searchOne(flexQuery);
        return appVersion == null ? null : appVersion.getLastVersionedTime();
    }

    /**
     * Preview the changes between the source and target environments.
     *
     * @param sourceEnvId Source Environment ID
     * @param targetEnvId Target Environment ID
     * @return List of model changes DTO
     */
    public List<ModelChangesDTO> previewBetweenEnv(@RequestParam Long sourceEnvId, @RequestParam Long targetEnvId) {
        DesignAppEnv sourceEnv = this.getById(sourceEnvId)
                .orElseThrow(() -> new IllegalArgumentException("The specified sourceEnvId does not exist! {0}", sourceEnvId));
        DesignAppEnv targetEnv = this.getById(targetEnvId)
                .orElseThrow(() -> new IllegalArgumentException("The specified targetEnvId does not exist! {0}", targetEnvId));
        Assert.isEqual(sourceEnv.getAppId(), targetEnv.getAppId(),
                "The source and target environments must belong to the same App!");
        LocalDateTime lastMergeTime = this.getLastMergeTime(sourceEnvId, targetEnvId);
        List<ModelChangesDTO> modelChanges = new ArrayList<>();
        for (String model : MetadataConstant.BASIC_METADATA_MODELS.keySet()) {
            ModelChangesDTO modelChangesDTO = versionControl.getModelChanges(sourceEnv, model, lastMergeTime);
            if (modelChangesDTO != null) {
                modelChanges.add(modelChangesDTO);
            }
        }
        return modelChanges;
    }

    /**
     * Merge the changes between the source and target environments.
     *
     * @param sourceEnvId Source Environment ID
     * @param targetEnvId Target Environment ID
     */
    @Transactional(rollbackFor = Exception.class)
    public void mergeBetweenEnv(@RequestParam Long sourceEnvId, @RequestParam Long targetEnvId) {
        DesignAppEnv targetEnv = this.getById(targetEnvId)
                .orElseThrow(() -> new IllegalArgumentException("The specified targetEnvId does not exist! {0}", targetEnvId));
        List<ModelChangesDTO> modelChanges = this.previewBetweenEnv(sourceEnvId, targetEnvId);
        Assert.notEmpty(modelChanges, "No changes found between the source env {0} and target env {1}!",
                sourceEnvId, targetEnvId);

        // Merge the changes to the target env
        this.mergeChangesToTargetEnv(targetEnv, modelChanges);

        // Create merge record
        DesignAppEnvMerge envMerge = new DesignAppEnvMerge();
        envMerge.setAppId(targetEnv.getAppId());
        envMerge.setSourceEnvId(sourceEnvId);
        envMerge.setTargetEnvId(targetEnvId);
        envMerge.setMergeContent(JsonMapper.objectToJsonNode(modelChanges));
        envMergeService.createOne(envMerge);
    }

    /**
     * Get the last merge time between the source and target environments.
     *
     * @param sourceEnvId Source Environment ID
     * @param targetEnvId Target Environment ID
     * @return Last merge time
     */
    private LocalDateTime getLastMergeTime(Long sourceEnvId, Long targetEnvId) {
        Filters filters = new Filters().eq(DesignAppEnvMerge::getSourceEnvId, sourceEnvId)
                .eq(DesignAppEnvMerge::getTargetEnvId, targetEnvId);
        Orders orders = Orders.ofDesc(DesignAppEnvMerge::getCreatedTime);
        FlexQuery flexQuery = new FlexQuery(filters, orders);
        flexQuery.setLimitSize(1);
        DesignAppEnvMerge envMerge = envMergeService.searchOne(flexQuery);
        return envMerge == null ? null : envMerge.getCreatedTime();
    }

    /**
     * Merge changes to target Env.
     * Update and delete by externalId, fill externalId using the id of source env.
     *
     * @param targetEnv           Target environment
     * @param modelChangesDTOList List of model changes
     */
    private void mergeChangesToTargetEnv(DesignAppEnv targetEnv, List<ModelChangesDTO> modelChangesDTOList) {
        for (ModelChangesDTO modelChangesDTO : modelChangesDTOList) {
            String versionedModel = modelChangesDTO.getModelName();
            if (!modelChangesDTO.getCreatedRows().isEmpty()) {
                List<Map<String, Object>> createdRows = modelChangesDTO.getCreatedRows().stream()
                        .map(RowChangeDTO::getCurrentData)
                        .peek(row -> {
                            row.put(VersionConstant.ENV_ID, targetEnv.getId());
                            row.put(ModelConstant.EXTERNAL_ID, row.get(ModelConstant.ID));
                            row.remove(ModelConstant.ID);
                        })
                        .toList();
                modelService.createList(versionedModel, createdRows);
            }
            if (!modelChangesDTO.getUpdatedRows().isEmpty()) {
                List<Map<String, Object>> updatedRows = modelChangesDTO.getUpdatedRows().stream()
                        .map(RowChangeDTO::getCurrentData)
                        .peek(row -> {
                            row.put(VersionConstant.ENV_ID, targetEnv.getId());
                            row.put(ModelConstant.EXTERNAL_ID, row.get(ModelConstant.ID));
                            row.remove(ModelConstant.ID);
                        })
                        .toList();
                modelService.updateByExternalId(versionedModel, updatedRows);
            }
            if (!modelChangesDTO.getDeletedRows().isEmpty()) {
                List<Serializable> deletedIds = modelChangesDTO.getDeletedRows().stream()
                        .map(RowChangeDTO::getRowId)
                        .toList();
                modelService.deleteByExternalIds(versionedModel, deletedIds);
            }
        }
    }
}