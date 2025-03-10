package info.openmeta.framework.web.controller;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.DataMask;
import info.openmeta.framework.orm.domain.*;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.service.FileService;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.framework.web.vo.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Model Common Controller.
 * The convertType of ModelController is `REFERENCE` by default,
 * means that the value of expandable fields will be converted to a reference object.
 * The value of ManyToOne/OneToOne field is a ModelReference object.
 * The value of Option/MultiOption field is a OptionReference object or List<OptionReference> object.
 *
 * @param <K> primary key type
 */
@Tag(name = "Model APIs", description = "Common model APIs, including create, read, update, delete, copy, search, etc.")
@RestController
@RequestMapping("/{modelName}")
public class ModelController<K extends Serializable> {

    @Autowired
    private ModelService<K> modelService;

    @Autowired
    private FileService fileService;

    /**
     * The size of operation data in a single API call cannot exceed the MAX_BATCH_SIZE.
     *
     * @param size data size
     */
    private void validateBatchSize(int size) {
        Assert.isTrue(size <= BaseConstant.MAX_BATCH_SIZE,
                "The size of operation data cannot exceed the maximum {0} limit.", BaseConstant.MAX_BATCH_SIZE);
    }

    /**
     * The ids of API request cannot contain null values, and cannot exceed the maximum number limit.
     *
     * @param ids id list
     */
    private void validateIds(List<K> ids) {
        Assert.allNotNull(ids, "ids cannot contain null values: {0}", ids);
        validateBatchSize(ids.size());
    }

    /**
     * Create a single row and return the id.
     *
     * @param modelName model name
     * @param row       data row to be created
     * @return id
     */
    @PostMapping(value = "/createOne")
    @Operation(description = "Create one row and return the id.")
    @DataMask
    public ApiResponse<K> createOne(@PathVariable String modelName,
                                    @RequestBody Map<String, Object> row) {
        return ApiResponse.success(modelService.createOne(modelName, row));
    }

    /**
     * Create a single row, fetch the row data after creation.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName model name
     * @param row       data row to be created
     * @return row data with id and other latest field values
     */
    @PostMapping(value = "/createOneAndFetch")
    @Operation(description = "Create one row and fetch the latest values from database.")
    @DataMask
    public ApiResponse<Map<String, Object>> createOneAndFetch(@PathVariable String modelName,
                                                              @RequestBody Map<String, Object> row) {
        return ApiResponse.success(modelService.createOneAndFetch(modelName, row, ConvertType.REFERENCE));
    }

    /**
     * Create multiple rows and return the IDs.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName model name
     * @param rows      data rows to be created
     * @return id list
     */
    @PostMapping("/createList")
    @Operation(description = "Create multiple rows and return the ID list.")
    public ApiResponse<List<K>> createList(@PathVariable String modelName,
                                           @RequestBody List<Map<String, Object>> rows) {
        this.validateBatchSize(rows.size());
        return ApiResponse.success(modelService.createList(modelName, rows));
    }

    /**
     * Create multiple rows, fetch the rows after creation.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName model name
     * @param rows      data rows to be created
     * @return row data list with id and other latest field values
     */
    @PostMapping("/createListAndFetch")
    @Operation(description = "Create multiple rows and fetch the latest values from database.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> createListAndFetch(@PathVariable String modelName,
                                                                     @RequestBody List<Map<String, Object>> rows) {
        this.validateBatchSize(rows.size());
        return ApiResponse.success(modelService.createListAndFetch(modelName, rows, ConvertType.REFERENCE));
    }

    /**
     * Get one row by id.
     * If the fields is not specified, all accessible fields as the default.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName     model name
     * @param getByIdParams Get one row by ID parameters
     * @return data row
     */
    @PostMapping(value = "/getById")
    @Operation(description = "Get one row by ID.")
    @DataMask
    public ApiResponse<Map<String, Object>> getById(@PathVariable String modelName,
                                                    @RequestBody GetByIdParams getByIdParams) {
        ContextHolder.getContext().setEffectiveDate(getByIdParams.getEffectiveDate());
        Assert.notNull(getByIdParams.getId(), "The ID of the data to be read cannot be null!");
        K id = IdUtils.formatId(modelName, getByIdParams.getId());
        SubQueries subQueries = new SubQueries();
        if (!CollectionUtils.isEmpty(getByIdParams.getSubQueries())) {
            subQueries.setQueryMap(getByIdParams.getSubQueries());
        }
        Optional<Map<String, Object>> row = modelService.getById(modelName, id,
                getByIdParams.getFields(), subQueries, ConvertType.REFERENCE);
        return ApiResponse.success(row.orElse(null));
    }

    /**
     * Get multiple rows by ids.
     * If the fields is not specified, all accessible fields as the default.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName     model name
     * @param getByIdsParams Get multiple rows by IDs parameters
     * @return List<Map> of multiple data
     */
    @PostMapping(value = "/getByIds")
    @Operation(description = "Get multiple rows by IDs.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> getByIds(@PathVariable String modelName,
                                                           @RequestBody GetByIdsParams getByIdsParams) {
        ContextHolder.getContext().setEffectiveDate(getByIdsParams.getEffectiveDate());
        List<K> ids = IdUtils.formatIds(modelName, getByIdsParams.getIds());
        this.validateIds(ids);
        SubQueries subQueries = new SubQueries();
        if (!CollectionUtils.isEmpty(getByIdsParams.getSubQueries())) {
            subQueries.setQueryMap(getByIdsParams.getSubQueries());
        }
        return ApiResponse.success(modelService.getByIds(modelName, ids,
                getByIdsParams.getFields(), subQueries, ConvertType.REFERENCE));
    }

    /**
     * Get the copyable fields value by ID, no data inserted.
     *
     * @param modelName model name
     * @param id        source data id
     * @return map of copyable field values
     */
    @GetMapping("/getCopyableFields")
    @Operation(description = "Get the copyable fields value by ID, no data inserted.")
    @Parameter(name = "id", description = "Data ID to be copied.", schema = @Schema(type = "number"))
    @DataMask
    public ApiResponse<Map<String, Object>> getCopyableFields(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.getCopyableFields(modelName, id));
    }

    /**
     * Get the original value for masking field based on the id and field name.
     *
     * @param modelName model name
     * @param id        data id
     * @param field     masking field name
     * @return unmasked field value
     */
    @GetMapping("/getUnmaskedField")
    @Operation(description = "Get the original value for masking field.")
    @Parameters({
            @Parameter(name = "id", description = "Data ID to be read", schema = @Schema(type = "number")),
            @Parameter(name = "field", description = "The masking field name to get the original value"),
            @Parameter(name = "effectiveDate", description = "Effective date for timeline model.")
    })
    public ApiResponse<String> getUnmaskedField(@PathVariable String modelName,
                                                @RequestParam K id,
                                                @RequestParam String field,
                                                @RequestParam(required = false) LocalDate effectiveDate) {
        ContextHolder.getContext().setEffectiveDate(effectiveDate);
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.getUnmaskedField(modelName, id, field));
    }

    /**
     * Get the original values for masking fields based on the id and field names.
     *
     * @param modelName model name
     * @param id        data id
     * @param fields    masking field names
     * @return unmasked field values map, {fieldName: fieldValue}
     */
    @GetMapping("/getUnmaskedFields")
    @Operation(description = "Get the original values for multiple masking fields.")
    @Parameters({
            @Parameter(name = "id", description = "Data ID to be read", schema = @Schema(type = "number")),
            @Parameter(name = "fields", description = "The masking field list to get the original values"),
            @Parameter(name = "effectiveDate", description = "Effective date for timeline model.")
    })
    public ApiResponse<Map<String, Object>> getUnmaskedFields(@PathVariable String modelName,
                                                              @RequestParam K id,
                                                              @RequestParam List<String> fields,
                                                              @RequestParam(required = false) LocalDate effectiveDate) {
        ContextHolder.getContext().setEffectiveDate(effectiveDate);
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.getUnmaskedFields(modelName, id, fields));
    }

    /**
     * Update one row by id.
     *
     * @param modelName model name
     * @param row       data row to be updated
     * @return true / Exception
     */
    @PostMapping(value = "/updateOne")
    @Operation(description = "Update one row by ID. Return true on success.")
    @DataMask
    public ApiResponse<Boolean> updateOne(@PathVariable String modelName,
                                          @RequestBody Map<String, Object> row) {
        Assert.notNull(row.get("id"), "`id` cannot be null or missing when updating data!");
        IdUtils.formatMapId(modelName, row);
        return ApiResponse.success(modelService.updateOne(modelName, row));
    }

    /**
     * Update one row by id, and fetch the latest field values.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName model name
     * @param row       data row to be updated
     * @return row map fetched from the database after updating
     */
    @PostMapping(value = "/updateOneAndFetch")
    @Operation(description = "Update one row by ID, and fetch the latest values from database.")
    @DataMask
    public ApiResponse<Map<String, Object>> updateOneAndFetch(@PathVariable String modelName,
                                                               @RequestBody Map<String, Object> row) {
        Assert.notEmpty(row, "The data to be updated cannot be empty!");
        Assert.notNull(row.get("id"), "`id` cannot be null or missing when updating data!");
        IdUtils.formatMapId(modelName, row);
        return ApiResponse.success(modelService.updateOneAndFetch(modelName, row, ConvertType.REFERENCE));
    }

    /**
     * Update multiple rows by ids. Each row in the list can have different fields.
     *
     * @param modelName model name
     * @param rows      data rows to be updated
     * @return true / Exception
     */
    @PostMapping(value = "/updateList")
    @Operation(description = "Update multiple rows by ID. Return true on success.")
    public ApiResponse<Boolean> updateList(@PathVariable String modelName,
                                           @RequestBody List<Map<String, Object>> rows) {
        Assert.notEmpty(rows, "The data to be updated cannot be empty!");
        this.validateBatchSize(rows.size());
        IdUtils.formatMapIds(modelName, rows);
        return ApiResponse.success(modelService.updateList(modelName, rows));
    }

    /**
     * Update multiple rows by ids. Each row in the list can have different fields.
     * And fetch the latest field values.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName model name
     * @param rows      data rows to be updated
     * @return rows fetched from the database after updating
     */
    @PostMapping(value = "/updateListAndFetch")
    @Operation(description = "Update multiple rows by ID, and fetch the latest values from database.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> updateListAndFetch(@PathVariable String modelName,
                                                                     @RequestBody List<Map<String, Object>> rows) {
        Assert.notEmpty(rows, "The data to be updated cannot be empty!");
        this.validateBatchSize(rows.size());
        IdUtils.formatMapIds(modelName, rows);
        return ApiResponse.success(modelService.updateListAndFetch(modelName, rows, ConvertType.REFERENCE));
    }

    /**
     * Bulk update data based on the filters, according to the specified field values map.
     *
     * @param modelName model name
     * @param bulkUpdateParams bulk update parameters
     * @return number of affected rows
     */
    @PostMapping(value = "/updateByFilter")
    @Operation(description = "Batch update data according to the filters, within the current user's permission scope.")
    public ApiResponse<Integer> updateByFilter(@PathVariable String modelName,
                                               @RequestBody BulkUpdateParams bulkUpdateParams) {
        Map<String, Object> values = bulkUpdateParams.getValues();
        Assert.notEmpty(values, "The updated data cannot be empty!");
        ContextHolder.getContext().setEffectiveDate(bulkUpdateParams.getEffectiveDate());
        Integer count = modelService.updateByFilter(modelName, bulkUpdateParams.getFilters(), values);
        return ApiResponse.success(count);
    }

    /**
     * Delete one row by id.
     * All slices related to this `id` will be deleted if the model is a timeline model.
     *
     * @param modelName model name
     * @param id        data id
     * @return true / Exception
     */
    @PostMapping(value = "/deleteById")
    @Operation(description = "Delete one row by ID. All slices related to this `ID` will be deleted for timeline model.")
    @Parameter(name = "id", description = "ID of the data to be deleted.", schema = @Schema(type = "number"))
    public ApiResponse<Boolean> deleteById(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.deleteById(modelName, id));
    }

    /**
     * Delete one slice of timeline model by `sliceId`, the primary key of timeline model.
     *
     * @param modelName model name
     * @param sliceId   data id
     * @return True / Exception
     */
    @PostMapping(value = "/deleteBySliceId")
    @Operation(description = "Delete one slice of the timeline model by `sliceId`.")
    @Parameter(name = "sliceId", description = "`sliceId` of the timeline slice data to delete.", schema = @Schema(type = "number"))
    public ApiResponse<Boolean> deleteBySliceId(@PathVariable String modelName, @RequestParam Serializable sliceId) {
        return ApiResponse.success(modelService.deleteBySliceId(modelName, sliceId));
    }

    /**
     * Delete multiple rows by ids.
     *
     * @param modelName model name
     * @param ids       data ids
     * @return True / Exception
     */
    @PostMapping(value = "/deleteByIds")
    @Operation(description = "Delete multiple rows by IDs.")
    @Parameter(name = "ids", description = "IDs of the data to be deleted.")
    public ApiResponse<Boolean> deleteByIds(@PathVariable String modelName,
                                            @RequestParam List<K> ids) {
        this.validateIds(ids);
        ids = IdUtils.formatIds(modelName, ids);
        return ApiResponse.success(modelService.deleteByIds(modelName, ids));
    }

    /**
     * Copy a single row based on id, and return the id of the new row.
     *
     * @param modelName model name
     * @param id        data source ID to be copied.
     * @return id of the new data
     */
    @PostMapping(value = "/copyById")
    @Operation(description = "Copy one row based on ID, and return the ID of the new row.")
    @Parameter(name = "id", description = "Data ID to be copied.", schema = @Schema(type = "number"))
    @DataMask
    public ApiResponse<K> copyById(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.copyById(modelName, id));
    }

    /**
     * Copy a single row based on id, and fetch the new row.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName model name
     * @param id        data source id
     * @return new row data
     */
    @PostMapping(value = "/copyByIdAndFetch")
    @Operation(description = "Copy one row based on ID, and fetch the new row.")
    @Parameter(name = "id", description = "Data ID to be copied.", schema = @Schema(type = "number"))
    @DataMask
    public ApiResponse<Map<String, Object>> copyByIdAndFetch(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.copyByIdAndFetch(modelName, id, ConvertType.REFERENCE));
    }

    /**
     * Copy multiple rows based on ids, and return the ids of the new rows.
     *
     * @param modelName model name
     * @param ids       data source ids
     * @return ids of the new data
     */
    @PostMapping(value = "/copyByIds")
    @Operation(description = "Copy multiple rows based on IDs, and return the new data IDs.")
    @Parameter(name = "ids", description = "Data IDs to be copied.")
    @DataMask
    public ApiResponse<List<K>> copyByIds(@PathVariable String modelName, @RequestParam List<K> ids) {
        this.validateIds(ids);
        ids = IdUtils.formatIds(modelName, ids);
        return ApiResponse.success(modelService.copyByIds(modelName, ids));
    }

    /**
     * Copy multiple rows based on ids, and fetch the new rows.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     *
     * @param modelName model name
     * @param ids       source data ids
     * @return new rows data
     */
    @PostMapping(value = "/copyByIdsAndFetch")
    @Operation(description = "Copy multiple rows based on ids, and fetch the new rows.")
    @Parameter(name = "ids", description = "Data IDs to be copied.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> copyByIdsAndFetch(@PathVariable String modelName,
                                                                    @RequestParam List<K> ids) {
        this.validateIds(ids);
        ids = IdUtils.formatIds(modelName, ids);
        return ApiResponse.success(modelService.copyByIdsAndFetch(modelName, ids, ConvertType.REFERENCE));
    }

    /**
     * Query data with pagination. The page size cannot exceed the MAX_BATCH_SIZE.
     * Set `convertType = REFERENCE` to get the reference object of expandable fields.
     * Support SUM, AVG, MIN, MAX, COUNT aggregation queries.
     *
     * @param modelName model name
     * @param queryParams  aggregation query parameters
     * @return data list in the page
     */
    @PostMapping(value = "/searchPage")
    @Operation(description = "Paging aggregation query parameters, including fields, filters, orders, pageNumber, pageSize" +
                    "groupBy, aggFunctions, subQueries, etc. Use the backend default value when not specified.")
    @DataMask
    public ApiResponse<Page<Map<String, Object>>> searchPage(@PathVariable String modelName,
                                                             @RequestBody(required = false) QueryParams queryParams) {
        if (queryParams == null) {
            queryParams = new QueryParams();
        }
        FlexQuery flexQuery = QueryParams.convertParamsToFlexQuery(queryParams);
        Page<Map<String, Object>> page = Page.of(queryParams.getPageNumber(), queryParams.getPageSize());
        return ApiResponse.success(modelService.searchPage(modelName, flexQuery, page));
    }

    /**
     * Query data list without pagination, the `limit` is default to DEFAULT_PAGE_SIZE.
     * The OneToOne/ManyToOne field value is the `displayName` of the related model.
     * Support SUM, AVG, MIN, MAX, COUNT aggregation queries.
     *
     * @param modelName model name
     * @param searchListParams  Aggregation query parameter
     * @return data list
     */
    @PostMapping(value = "/searchList")
    @Operation(description = "Query the data list based on the specified fields, filters, orders, limitSize, " +
            "aggFunctions, and subQueries. Default limit to 50.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> searchList(@PathVariable String modelName,
                                                             @RequestBody(required = false) SearchListParams searchListParams) {
        FlexQuery flexQuery = SearchListParams.convertParamsToFlexQuery(searchListParams);
        return ApiResponse.success(modelService.searchList(modelName, flexQuery));
    }

    /**
     * Query the displayNames based on the match field, filters, orders, limitSize.
     * Default limit to 10.
     *
     * @param modelName model name
     * @param searchNameParams SearchNameParams
     * @return List of data with displayName and additional fields
     */
    @PostMapping(value = "/searchName")
    @Operation(description = "Query the displayNames based on the match field, filters, orders, limitSize. " +
            "Default limit to 10.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> searchName(@PathVariable String modelName,
                                                             @RequestBody(required = false) SearchNameParams searchNameParams) {
        FlexQuery flexQuery = SearchNameParams.convertParamsToFlexQuery(searchNameParams);
        return ApiResponse.success(modelService.searchName(modelName, flexQuery));
    }

    /**
     * Simple aggregation query params by `filters` and `aggFunctions`.
     * SUM, AVG, MIN, MAX, COUNT aggregation queries, such as `["SUM", "amount"]`, or `[["SUM", "amount"], [], ...]`.
     * Each key of the result is a camel case string concatenated with the functionName + fieldName, e.g., `sumAmount`.
     * Use the searchPage interface when grouping or paging is needed.
     *
     * @param modelName model name
     * @param simpleAggParams Simple aggregation query parameters
     * @return Result map.
     */
    @PostMapping(value = "/searchSimpleAgg")
    @Operation(description = """
            Simple aggregation query params. Pure SUM, AVG, MIN, MAX, COUNT aggregate query, like `["SUM", "amount"]`
            or `[["SUM", "amount"], [], ...]`, the return key is `sumAmount`.""")
    @DataMask
    public ApiResponse<Map<String, Object>> searchSimpleAgg(@PathVariable String modelName,
                                                            @RequestBody SimpleAggParams simpleAggParams) {
        ContextHolder.getContext().setEffectiveDate(simpleAggParams.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(simpleAggParams.getFilters());
        Assert.notTrue(AggFunctions.isEmpty(simpleAggParams.getAggFunctions()), "`aggFunctions` cannot be null!");
        // Set AggFunction parameters
        flexQuery.setAggFunctions(simpleAggParams.getAggFunctions());
        return ApiResponse.success(modelService.searchOne(modelName, flexQuery));
    }

    /**
     * Query the PivotTable; without pagination, the data is limited to no more than DEFAULT_BATCH_SIZE records.
     *
     * @param modelName model name
     * @param queryParams  aggregation query parameters
     * @return PivotTable object
     */
    @PostMapping(value = "/searchPivot")
    @Operation(description = "Get the pivot table data based on the specified fields, filters, orders, groupBy, splitBy.")
    @DataMask
    public ApiResponse<PivotTable> searchPivot(@PathVariable String modelName,
                                               @RequestBody(required = false) QueryParams queryParams) {
        if (queryParams == null) {
            queryParams = new QueryParams();
        }
        FlexQuery flexQuery = QueryParams.convertParamsToFlexQuery(queryParams);
        flexQuery.setSplitBy(queryParams.getSplitBy());
        return ApiResponse.success(modelService.searchPivot(modelName, flexQuery));
    }

    /**
     * Count a query. Support group counting by `groupBy` parameter, count the number of each group.
     * That is, the amount of data under each grouped field value. For example,
     * `groupBy=name,code,sequence & orders=sequence`, the actual SQL statement is:
     * `SELECT name, code, sequence, count(*) AS count FROM table_name GROUP BY name, code, sequence ORDER BY sequence`
     *
     * @param modelName model name
     * @param countParams CountParams
     * @return Group counting results.
     */
    @PostMapping(value = "/count")
    @Operation(description = "Returns a count or group counting based on the specified `filter`, `groupBy`, and `orders`.")
    @DataMask
    public ApiResponse<Object> count(@PathVariable String modelName,
                                     @RequestBody(required = false) CountParams countParams) {
        if (countParams == null) {
            countParams = new CountParams();
        }
        ContextHolder.getContext().setEffectiveDate(countParams.getEffectiveDate());
        List<String> groupBy = countParams.getGroupBy();
        if (!CollectionUtils.isEmpty(groupBy)) {
            Assert.allNotBlank(groupBy, "`groupBy` cannot contain empty value: {0}", groupBy);
            FlexQuery flexQuery = new FlexQuery(countParams.getFilters(), countParams.getOrders());
            flexQuery.setFields(new HashSet<>(groupBy));
            flexQuery.setGroupBy(groupBy);
            flexQuery.setConvertType(ConvertType.TYPE_CAST);
            return ApiResponse.success(modelService.searchList(modelName, flexQuery));
        } else {
            return ApiResponse.success(modelService.count(modelName, countParams.getFilters()));
        }
    }

    /**
     * Get the fileInfo by modelName and rowId
     */
    @Operation(description = "Get the fileInfos by modelName and rowId")
    @GetMapping(value = "/getRowFiles")
    @Parameter(name = "rowId", description = "The row ID of the file belongs to")
    public ApiResponse<List<FileInfo>> getRowFiles(@PathVariable String modelName,
                                                   @RequestParam Serializable rowId) {
        Assert.notNull(rowId, "rowId cannot be null.");
        return ApiResponse.success(fileService.getRowFiles(modelName, rowId));
    }

    /**
     * Upload a file to the specified model, and return the fileInfo.
     *
     * @param modelName The model name of the file belongs to
     * @param file The file to be uploaded
     * @return The fileInfo object
     */
    @Operation(description = "Upload a file to the specified model to bind with a field.")
    @PostMapping("/uploadFile")
    public ApiResponse<FileInfo> uploadFile(@PathVariable String modelName,
                                            @RequestParam MultipartFile file) {
        Assert.notTrue(file.isEmpty(), "The file to upload cannot be empty!");
        return ApiResponse.success(fileService.uploadFileToRow(modelName, null, file));
    }

    /**
     * Upload a file to the specified model and rowId, and return the fileInfo.
     *
     * @param modelName The model name of the file belongs to
     * @param rowId The row ID of the file belongs to
     * @param file The file to be uploaded
     * @return The fileInfo of the uploaded file
     */
    @Operation(description = "Upload a file to the specified model and row, and return the fileInfo.")
    @PostMapping("/uploadFileToRow")
    @Parameter(name = "rowId", description = "The row ID of the file belongs to")
    public ApiResponse<FileInfo> uploadFileToRow(@PathVariable String modelName,
                                                 @RequestParam Serializable rowId,
                                                 @RequestParam MultipartFile file) {
        Assert.notNull(rowId, "rowId cannot be null.");
        Assert.notTrue(file.isEmpty(), "The file to upload cannot be empty!");
        return ApiResponse.success(fileService.uploadFileToRow(modelName, rowId, file));
    }

}
