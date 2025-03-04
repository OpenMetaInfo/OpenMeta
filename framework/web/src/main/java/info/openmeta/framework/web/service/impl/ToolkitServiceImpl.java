package info.openmeta.framework.web.service.impl;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.encrypt.EncryptUtils;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.web.service.ToolkitService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * The implementation class for ToolkitService
 */
@Service
public class ToolkitServiceImpl implements ToolkitService {

    @Autowired
    private ModelService<?> modelService;

    /**
     * Get the dependent fields for stored cascaded and computed fields.
     *
     * @param modelName the name of the model
     * @param fields a set of field names that need to be recalculated
     * @return a set of dependent fields
     */
    private Set<String> getDependedFields(String modelName, Set<String> fields) {
        Collection<MetaField> metaFields;
        if (CollectionUtils.isEmpty(fields)) {
            metaFields = ModelManager.getModelFields(modelName);
        } else {
            metaFields = fields.stream().map(fieldName -> ModelManager.getModelField(modelName, fieldName)).collect(Collectors.toList());
        }
        // Get the dependent fields for stored cascaded and computed fields
        Set<String> dependedFields = new HashSet<>();
        metaFields.stream()
                .filter(metaField -> !metaField.isDynamic())
                .forEach(metaField -> {
                    if (StringUtils.isNotBlank(metaField.getCascadedField())) {
                        dependedFields.add(metaField.getDependentFields().getFirst());
                    } else if (metaField.isComputed()) {
                        dependedFields.addAll(metaField.getDependentFields());
                    }
                });
        return dependedFields;
    }

    /**
     * Recompute the stored calculation fields, including computed and cascaded fields.
     *
     * @param modelName model name
     * @param fields fields to be recomputed
     */
    @Override
    public void recompute(String modelName, Set<String> fields) {
        // TODO: Asynchronous task processing
        // Get the dependent fields for stored cascaded and computed fields
        Set<String> dependedFields = this.getDependedFields(modelName, fields);
        Assert.notEmpty(dependedFields, "No stored cascaded or computed fields need recalculation for model {0}!", modelName);
        dependedFields.addAll(ModelManager.isTimelineModel(modelName) ?
                Sets.newHashSet(ModelConstant.ID, ModelConstant.SLICE_ID) : Sets.newHashSet(ModelConstant.ID));
        // Construct FlexQuery to read dependent fields for pagination
        FlexQuery flexQuery = new FlexQuery(dependedFields).acrossTimelineData();
        Page<Map<String, Object>> page = Page.ofCursorPage(BaseConstant.DEFAULT_BATCH_SIZE);
        // Paginate requests for dependent fields and trigger updates by batch processing each page
        do {
            page = modelService.searchPage(modelName, flexQuery, page);
            // TODO: When both the main model and the cascaded model are timeline models, the calculation of
            //  cascaded data can only proceed after getting the `effectiveStartDate` from the main model data,
            //  which is then used as the `effectiveDate` to fetch data from the cascaded model.
            if (!page.getRows().isEmpty()) {
                modelService.updateList(modelName, page.getRows());
            }
        } while (page.toNext());
    }

    /**
     * Encrypts historical plaintext data after the field is set to `encrypted=true`.
     *
     * @param modelName model name
     * @param fieldName field to encrypt historical plaintext data.
     * @return the number of rows fixed
     */
    @Override
    public Long fixUnencryptedData(String modelName, String fieldName) {
        // TODO: Asynchronous task processing
        MetaField metaField = ModelManager.getModelField(modelName, fieldName);
        Assert.isTrue(metaField.isEncrypted(), "The field {0} of model {1} is not an encrypted field!", fieldName, modelName);
        long fixedCount = 0L;
        // Construct query to read required fields for pagination
        Set<String> readFields = ModelManager.isTimelineModel(modelName) ?
                Sets.newHashSet(ModelConstant.ID, ModelConstant.SLICE_ID) : Sets.newHashSet(ModelConstant.ID);
        readFields.add(fieldName);
        FlexQuery flexQuery = new FlexQuery(readFields).acrossTimelineData();
        // Get the original data from database without expansion or conversion.
        flexQuery.setConvertType(ConvertType.ORIGINAL);
        Page<Map<String, Object>> page = Page.ofCursorPage(BaseConstant.DEFAULT_BATCH_SIZE);
        // Paginate requests for data and process each page
        do {
            page = modelService.searchPage(modelName, flexQuery, page);
            if (!page.getRows().isEmpty()) {
                fixedCount += this.decryptAndUpdate(modelName, fieldName, page.getRows());
            }
        } while (page.toNext());
        return fixedCount;
    }

    /**
     * Extracts database field values, decrypts them, and filters out plaintext data that cannot be decrypted,
     * then invokes update method to encrypt these plaintext data.
     *
     * @param modelName the name of the model
     * @param fieldName the field name of the historical data to be fixed
     * @param rows the database rows obtained through pagination
     * @return the number of historical rows fixed in the current page
     */
    private Integer decryptAndUpdate(String modelName, String fieldName, List<Map<String, Object>> rows) {
        // Extract a map of ciphertext: index-ciphertext for batch decryption, ignoring null and empty strings.
        Map<Integer, String> ciphertextMap = ListUtils.extractValueIndexMap(rows, fieldName);
        if (!CollectionUtils.isEmpty(ciphertextMap)) {
            // Perform batch decryption on the original values, obtaining a map of ciphertext-plaintext pairs.
            // Values not in this map are plaintext
            Map<Integer, String> decryptedValues = EncryptUtils.decrypt(ciphertextMap);
            List<Map<String, Object>> plaintextRows = new ArrayList<>();
            for (int i = 0; i < rows.size(); i++) {
                if (!decryptedValues.containsKey(i)) {
                    plaintextRows.add(rows.get(i));
                }
            }
            if (!CollectionUtils.isEmpty(plaintextRows)) {
                // Update the plaintext data to trigger encryption storage
                modelService.updateList(modelName, plaintextRows);
                return plaintextRows.size();
            }
        }
        return 0;
    }

}
