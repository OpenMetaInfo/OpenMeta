package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.web.dto.MetadataUpgradePackage;
import info.openmeta.starter.metadata.message.InnerBroadcastProducer;
import info.openmeta.starter.metadata.message.dto.InnerBroadcastMessage;
import info.openmeta.starter.metadata.message.enums.InnerBroadcastType;
import info.openmeta.starter.metadata.constant.MetadataConstant;
import info.openmeta.starter.metadata.service.MetadataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Metadata upgrade service implementation.
 */
@Service
public class MetadataServiceImpl implements MetadataService {

    @Autowired
    private ModelService<Serializable> modelService;

    @Autowired
    private InnerBroadcastProducer innerBroadcastProducer;

    /**
     * The size of operation data in a single API call cannot exceed the MAX_BATCH_SIZE.
     *
     * @param size data size
     */
    private void validateBatchSize(int size) {
        Assert.isTrue(size <= BaseConstant.MAX_BATCH_SIZE,
                "The size of operation data cannot exceed the maximum {0} limit.",
                BaseConstant.MAX_BATCH_SIZE);
    }

    /**
     * Validate if the model is enabled for version control.
     */
    private void validateRuntimeModel(String modelName) {
        Assert.isTrue(MetadataConstant.BASIC_METADATA_MODELS.containsValue(modelName),
                "Model {0} is not enabled for version control, and the upgrade API cannot be invoked.", modelName);
    }

    /**
     * Create metadata.
     *
     * @param modelName The name of the model
     * @param createRows The list of metadata to be created
     */
    private void createMetadata(String modelName, List<Map<String, Object>> createRows) {
        if (!CollectionUtils.isEmpty(createRows)) {
            this.validateBatchSize(createRows.size());
            modelService.createList(modelName, createRows);
        }
    }

    /**
     * Update metadata by unique id
     *
     * @param modelName The name of the model
     * @param updateRows The list of data to be updated
     */
    private void updateById(String modelName, List<Map<String, Object>> updateRows) {
        if (!CollectionUtils.isEmpty(updateRows)) {
            this.validateBatchSize(updateRows.size());
            modelService.updateList(modelName, updateRows);
        }
    }

    /**
     * Delete metadata by unique id
     *
     * @param modelName The name of the model
     * @param ids The list of codes for the data to be deleted
     */
    private void deleteById(String modelName, List<Serializable> ids) {
        if (!CollectionUtils.isEmpty(ids)) {
            this.validateBatchSize(ids.size());
            modelService.deleteList(modelName, ids);
        }
    }

    /**
     * Upgrades the metadata of multiple models, all within a single transaction
     * to avoid refreshing the model pool repeatedly and missing dependency data.
     *
     * @param metadataPackages the metadata packages to upgrade
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void upgradeMetadata(List<MetadataUpgradePackage> metadataPackages) {
        metadataPackages.forEach(modelPackage -> {
            String modelName = modelPackage.getModelName();
            this.validateRuntimeModel(modelName);
            // create
            this.createMetadata(modelName, modelPackage.getCreateRows());
            // update
            this.updateById(modelName, modelPackage.getUpdateRows());
            // delete
            this.deleteById(modelName, modelPackage.getDeleteIds());
        });
    }

    /**
     * Reload metadata.
     * The current replica will be unavailable if an exception occurs during the reload,
     * and the metadata needs to be fixed and reloaded.
     */
    @Override
    public void reloadMetadata() {
        // Send an inner broadcast to reload the metadata of replica containers.
        InnerBroadcastMessage message = new InnerBroadcastMessage();
        message.setBroadcastType(InnerBroadcastType.RELOAD_METADATA);
        message.setContext(ContextHolder.cloneContext());
        innerBroadcastProducer.sendInnerBroadcast(message);
    }
}
