package info.openmeta.framework.web.task.handlers;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.SkipPermissionCheck;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.encrypt.EncryptUtils;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.web.task.AsyncTaskHandler;
import info.openmeta.framework.web.task.AsyncTaskHandlerList;
import info.openmeta.framework.web.task.params.PlaintextToCiphertextParams;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;
import static info.openmeta.framework.orm.constant.ModelConstant.SLICE_ID;

/**
 * Ciphertext to Plaintext Handler
 */
@Component
public class PlaintextToCiphertextHandler implements AsyncTaskHandler<PlaintextToCiphertextParams> {

    @Autowired
    private ModelService<?> modelService;

    /**
     * Get the code of the asynchronous task handler.
     * @return The code of the asynchronous task handler.
     */
    @Override
    public String getAsyncTaskHandlerCode() {
        return AsyncTaskHandlerList.PLAINTEXT_TO_CIPHERTEXT.getCode();
    }

    /**
     * Get the parameter type required by the current handler.
     * @return The class type of the parameters.
     */
    @Override
    public Class<PlaintextToCiphertextParams> getParamsType() {
        return PlaintextToCiphertextParams.class;
    }

    /**
     * Validate the data integrity of the asynchronous task parameters.
     * @param taskParams The parameters of the task to validate.
     */
    @Override
    public void validateParams(PlaintextToCiphertextParams taskParams) {
        Assert.notBlank(taskParams.getModel(),
                "Asynchronous task {0} model name parameter cannot be empty!", getAsyncTaskHandlerCode());
        Assert.allNotBlank(taskParams.getFields(),
                "Asynchronous task {0} fields parameter cannot be empty or contain null values! {1}",
                getAsyncTaskHandlerCode(), taskParams.getFields());
        Assert.allNotNull(taskParams.getIds(),
                "Asynchronous task {0} IDs parameter cannot be empty or contain null values! {1}",
                getAsyncTaskHandlerCode(), taskParams.getIds());
        for (String field : taskParams.getFields()) {
            MetaField metaField = ModelManager.getModelField(taskParams.getModel(), field);
            Assert.isTrue(metaField.isEncrypted(),
                    "Field {0} of model {1} is not an encrypted field!", field, taskParams.getModel());
        }
    }

    /**
     * Execute the asynchronous task.
     * @param taskParams The parameters for executing the task.
     */
    @Override
    @SkipPermissionCheck
    public void execute(PlaintextToCiphertextParams taskParams) {
        // Construct the pagination query for reading dependent fields.
        Set<String> readFields = ModelManager.isTimelineModel(taskParams.getModel()) ? Sets.newHashSet(ID, SLICE_ID) : Sets.newHashSet(ID);
        readFields.addAll(taskParams.getFields());
        Filters filters = Filters.in(ID, taskParams.getIds());
        FlexQuery flexQuery = new FlexQuery(readFields, filters).acrossTimelineData();
        // Get the original value from the database.
        flexQuery.setConvertType(ConvertType.ORIGINAL);
        List<Map<String, Object>> rows = modelService.searchList(taskParams.getModel(), flexQuery);
        taskParams.getFields().forEach(field -> {
            List<Map<String, Object>> plaintextRows = this.getPlaintextRows(field, rows);
            if (!CollectionUtils.isEmpty(plaintextRows)) {
                modelService.updateList(taskParams.getModel(), plaintextRows);
            }
        });
    }

    /**
     * Get the plaintext data.
     *
     * @param fieldName The name of the field that needs historical data correction.
     * @param rows The paginated database rows.
     * @return A list of plaintext data extracted from the specified field in the provided rows.
     */
    private List<Map<String, Object>> getPlaintextRows(String fieldName, List<Map<String, Object>> rows) {
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
            return plaintextRows;
        } else {
            return Collections.emptyList();
        }
    }
}
