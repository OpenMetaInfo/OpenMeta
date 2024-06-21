package info.openmeta.framework.orm.utils;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.SpringContextUtils;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.service.impl.ModelServiceImpl;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ReflectionUtils;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.*;

/**
 * Reflectively call the methods of the Model Service
 */
public abstract class ReflectTool {

    private ReflectTool() {}

    /**
     * Get the `modelService`
     * @return modelService
     */
    private static <K extends Serializable> ModelService<K> getModelService() {
        return SpringContextUtils.getBeanByClass(ModelServiceImpl.class);
    }

    /**
     * Get the entity service of specified model
     * @param modelName model name
     * @return entityService
     */
    private static <T extends BaseModel, K extends Serializable> EntityService<T, K> getEntityService(String modelName) {
        return SpringContextUtils.getBeanByName(StringTools.lowerCaseFirstLetter(modelName + "ServiceImpl"));
    }

    /**
     * Get the ids of the specified model.
     *
     * @param modelName model name
     * @param filters filters
     * @return ids list
     */
    public static List<Serializable> getIds(String modelName, Filters filters) {
        return getModelService().getIds(modelName, filters);
    }

    /**
     * Get the ids for ManyToOne/OneToOne relational field.
     *
     * @param modelName model name
     * @param filters filters
     * @param fieldName relational field name
     * @return distinct ids for relational field
     */
    public static List<Serializable> getRelatedIds(String modelName, String fieldName, Filters filters) {
        return getModelService().getRelatedIds(modelName, filters, fieldName);
    }

    /**
     * Batch get the displayNames of the specified ids of the current model: {id: displayName}.
     *
     * @param modelName model name
     * @param ids id list
     * @param displayFields specified display field list, if not specified, use the default displayName configuration of the model.
     * @return displayNames Map
     */
    public static Map<Serializable, String> getDisplayNames(String modelName, List<Serializable> ids, List<String> displayFields) {
        return getModelService().getDisplayNames(modelName, ids, displayFields);
    }

    /**
     * Create data list
     *
     * @param rows data list
     * @return enhanced List<Map> data list
     */
    public static List<Serializable> createList(String modelName, List<Map<String, Object>> rows) {
        if (CollectionUtils.isEmpty(rows)) {
            return Collections.emptyList();
        }
        return getModelService().createList(modelName, rows);
    }

    /**
     * Batch update data according to the id field in the list
     *
     * @param rows data list
     * @return enhanced List<Map> data list
     */
    public static boolean updateList(String modelName, List<Map<String, Object>> rows) {
        if (CollectionUtils.isEmpty(rows)) {
            return false;
        }
        return getModelService().updateList(modelName, rows);
    }

    /**
     * Call the read method of the specified model
     *
     * @param modelName model name
     * @param ids id list
     * @param fields field list to read
     * @param convertType format type
     * @return List<Map> data list
     */
    public static List<Map<String, Object>> readList(String modelName, List<Serializable> ids, Set<String> fields, ConvertType convertType) {
        if (CollectionUtils.isEmpty(ids)) {
            return new ArrayList<>();
        }
        return getModelService().readList(modelName, ids, fields, convertType);
    }

    /**
     * Call the searchMapList method of the specified model
     *
     * @param modelName model name
     * @param flexQuery flexQuery
     * @return Search result
     */
    public static List<Map<String, Object>> searchMapList(String modelName, FlexQuery flexQuery) {
        return getModelService().searchList(modelName, flexQuery);
    }

    /**
     * Call the searchPage method of the specified model
     *
     * @param modelName model name
     * @param flexQuery flexQuery
     * @param page page object
     * @return Search result
     */
    public static Page<Map<String, Object>> searchPage(String modelName, FlexQuery flexQuery, Page<Map<String, Object>> page) {
        return getModelService().searchPage(modelName, flexQuery, page);
    }

    /**
     * Call the delete method of the specified model
     *
     * @param modelName model name
     * @param ids id list
     * @return delete result
     */
    public static boolean deleteList(String modelName, List<Serializable> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return false;
        }
        return getModelService().deleteList(modelName, ids);
    }

    /**
     * Call the service method of the specified model through reflection
     *
     * @param modelName model name
     * @param methodName method name
     * @param params method parameters
     * @return method execution result
     */
    public static Object invoke(String modelName, String methodName, Object... params) {
        Class<?>[] paramsClass = null;
        if (params != null && params.length != 0) {
            paramsClass = new Class<?>[params.length];
            for (int i = 0; i < params.length; i++) {
                paramsClass[i] = params[i].getClass();
            }
        }
        EntityService<?, ?> entityService = getEntityService(modelName);
        Method method = ReflectionUtils.findMethod(entityService.getClass(), methodName, paramsClass);
        Assert.notNull(method, "Model {0} does not exist Method {1} with Parameter type {2}!", modelName, methodName, paramsClass);
        return ReflectionUtils.invokeMethod(method, entityService, params);
    }

}
