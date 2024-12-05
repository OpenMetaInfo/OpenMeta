package info.openmeta.framework.web.task.handlers;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.SkipPermissionCheck;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.web.task.AsyncTaskHandler;
import info.openmeta.framework.web.task.AsyncTaskHandlerList;
import info.openmeta.framework.web.task.params.RecomputeHandlerParams;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;
import static info.openmeta.framework.orm.constant.ModelConstant.SLICE_ID;

/**
 * Recompute Handler
 */
@Component
public class RecomputeModelFieldHandler implements AsyncTaskHandler<RecomputeHandlerParams> {

    @Autowired
    private ModelService<?> modelService;

    /**
     * Get the code of the asynchronous task handler.
     * @return The code of the asynchronous task handler.
     */
    @Override
    public String getAsyncTaskHandlerCode() {
        return AsyncTaskHandlerList.RECOMPUTE_MODEL_FIELD.getCode();
    }

    /**
     * Get the parameter type required by the current handler.
     * @return The class type of the parameters.
     */
    @Override
    public Class<RecomputeHandlerParams> getParamsType() {
        return RecomputeHandlerParams.class;
    }

    /**
     * Validate the data integrity of the asynchronous task parameters.
     * @param taskParams The parameters of the task to validate.
     */
    @Override
    public void validateParams(RecomputeHandlerParams taskParams) {
        Assert.notBlank(taskParams.getModel(),
                "Asynchronous task {0} model name parameter cannot be empty!", getAsyncTaskHandlerCode());
        Assert.allNotNull(taskParams.getIds(),
                "Asynchronous task {0} `ids` parameter cannot be null or contain null values! {1}",
                getAsyncTaskHandlerCode(), taskParams.getIds());
        if (!CollectionUtils.isEmpty(taskParams.getFields())) {
            taskParams.getFields().forEach(field -> {
                MetaField metaField = ModelManager.getModelField(taskParams.getModel(), field);
                Assert.isTrue(!metaField.isDynamic() && (metaField.isComputed() || StringUtils.isNotBlank(metaField.getCascadedField())),
                        "Asynchronous task {0} model {1} field {2} is not a stored computed field!",
                        getAsyncTaskHandlerCode(), taskParams.getModel(), field);
            });
        }
    }

    /**
     * Execute the asynchronous task.
     * @param taskParams The parameters for executing the task.
     */
    @Override
    @SkipPermissionCheck
    public void execute(RecomputeHandlerParams taskParams) {
        // Get the dependent fields for stored cascaded and computed fields
        Set<String> dependedFields = this.getDependedFields(taskParams.getModel(), taskParams.getFields());
        Assert.notEmpty(dependedFields,
                "No stored cascaded or computed fields need recalculation for model {0}!", taskParams.getModel());
        dependedFields.addAll(ModelManager.isTimelineModel(taskParams.getModel()) ? Sets.newHashSet(ID, SLICE_ID) : Sets.newHashSet(ID));
        // Construct FlexQuery to read dependent fields for pagination
        Filters filters = Filters.in(ID, taskParams.getIds());
        FlexQuery flexQuery = new FlexQuery(dependedFields, filters).acrossTimelineData();
        List<Map<String, Object>> rows = modelService.searchList(taskParams.getModel(), flexQuery);
        // TODO: When both the main model and the cascaded model are timeline models, the calculation of
        //  cascaded data can only proceed after getting the `effectiveStartDate` from the main model data,
        //  which is then used as the `effectiveDate` to fetch data from the cascaded model.
        if (!CollectionUtils.isEmpty(rows)) {
            modelService.updateList(taskParams.getModel(), rows);
        }
    }

    /**
     * Get the dependent fields for stored cascaded and computed fields.
     *
     * @param model the name of the model
     * @param fields a set of field names that need to be recalculated
     * @return a set of dependent fields
     */
    private Set<String> getDependedFields(String model, Set<String> fields) {
        Collection<MetaField> metaFields;
        if (CollectionUtils.isEmpty(fields)) {
            metaFields = ModelManager.getModelFields(model);
        } else {
            metaFields = fields.stream().map(field -> ModelManager.getModelField(model, field)).collect(Collectors.toList());
        }
        // Get the dependent fields for stored cascaded and computed fields
        Set<String> dependedFields = new HashSet<>();
        metaFields.stream().filter(metaField -> !metaField.isDynamic()).forEach(sysField -> {
            if (StringUtils.isNotBlank(sysField.getCascadedField())) {
                dependedFields.add(StringUtils.split(sysField.getCascadedField(), ".")[0]);
            } else if (sysField.isComputed()) {
                dependedFields.addAll(sysField.getDependentFields());
            }
        });
        return dependedFields;
    }
}
