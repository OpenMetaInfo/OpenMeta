package info.openmeta.starter.metadata.service.impl;

import com.google.common.collect.ImmutableMap;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.web.dto.MetadataUpgradePackage;
import info.openmeta.starter.metadata.service.MetadataUpgradeService;
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
public class MetadataUpgradeServiceImpl implements MetadataUpgradeService {

    /** Version control model mapping relationship between design time and runtime */
    Map<String, String> VERSION_CONTROL_MODELS = ImmutableMap.<String, String>builder()
            .put("DesignModel", "SysModel")
            .put("DesignField", "SysField")
            .put("DesignModelIndex", "SysModelIndex")
            .build();

    @Autowired
    private ModelService<Long> modelService;

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
        Assert.isTrue(VERSION_CONTROL_MODELS.containsValue(modelName),
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
     * Update metadata.
     *
     * @param modelName The name of the model
     * @param updateRows The list of data to be updated
     */
    private void updateByCode(String modelName, List<Map<String, Object>> updateRows) {
        if (!CollectionUtils.isEmpty(updateRows)) {
            this.validateBatchSize(updateRows.size());
            updateRows.forEach(row -> {
                // Update by code
                String code = (String) row.get(ModelConstant.CODE);
                row.remove(ModelConstant.CODE);
                modelService.updateByFilter(modelName, Filters.eq(ModelConstant.CODE, code), row);
            });
        }
    }

    /**
     * Delete metadata by code.
     *
     * @param modelName The name of the model
     * @param deleteCodes The list of codes for the data to be deleted
     */
    private void deleteByCode(String modelName, List<Serializable> deleteCodes) {
        if (!CollectionUtils.isEmpty(deleteCodes)) {
            this.validateBatchSize(deleteCodes.size());
            modelService.deleteByFilters(modelName, Filters.in(ModelConstant.CODE, deleteCodes));
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
            this.updateByCode(modelName, modelPackage.getUpdateRows());
            // delete
            this.deleteByCode(modelName, modelPackage.getDeleteCodes());
        });
    }
}
