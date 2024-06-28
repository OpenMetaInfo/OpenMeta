package info.openmeta.framework.web.controller;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.DataMask;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.*;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.web.response.ApiResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.*;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * Model Common Controller.
 * The convertType of ModelController is `KEY_AND_DISPLAY` by default, means
 * that the value of expandable fields
 * will be converted to the display value. Including ManyToOne, OneToOne,
 * Option, MultiOption fields.
 * The format of ManyToOne/OneToOne field value is `[id, displayName]`.
 * The format of Option/MultiOption field value is `[code, name]`.
 *
 * @param <K> primary key type
 */
@Tag(name = "Model APIs", description = "Common model APIs, including create, read, update, delete, copy, search, etc.")
@RestController
@RequestMapping("/{modelName}")
public class ModelController<K extends Serializable> {

    @Autowired
    private ModelService<K> modelService;

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
    @Operation(summary = "createOne: Create one row", description = "Create a single row and return the id.")
    @DataMask
    public ApiResponse<K> createOne(@PathVariable String modelName, @RequestBody Map<String, Object> row) {
        return ApiResponse.success(modelService.createOne(modelName, row));
    }

    /**
     * Create a single row, return the row map with id and other latest field values.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName model name
     * @param row       data row to be created
     * @return row data with id and other latest field values
     */
    @PostMapping(value = "/createOneAndReturn")
    @Operation(summary = "createOneAndReturn: Create one row and return result",
            description = "Create one row return the latest values from database.")
    @DataMask
    public ApiResponse<Map<String, Object>> createOneAndReturn(@PathVariable String modelName,
                                                               @RequestBody Map<String, Object> row) {
        return ApiResponse.success(modelService.createOneAndReturn(modelName, row, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Create multiple rows and return the IDs.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName model name
     * @param rows      data rows to be created
     * @return id list
     */
    @PostMapping("/createList")
    @Operation(summary = "createList: Batch creation", description = "Create multiple rows and return the id list.")
    public ApiResponse<List<K>> createList(@PathVariable String modelName,
                                           @RequestBody List<Map<String, Object>> rows) {
        this.validateBatchSize(rows.size());
        return ApiResponse.success(modelService.createList(modelName, rows));
    }

    /**
     * Create multiple rows, return the row list with id and other latest field values.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName model name
     * @param rows      data rows to be created
     * @return row data list with id and other latest field values
     */
    @PostMapping("/createListAndReturn")
    @Operation(summary = "createListAndReturn: Batch creation and return result",
            description = "Create multiple rows and return the latest values from database.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> createListAndReturn(@PathVariable String modelName,
                                                                      @RequestBody List<Map<String, Object>> rows) {
        this.validateBatchSize(rows.size());
        return ApiResponse.success(modelService.createListAndReturn(modelName, rows, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Read one row by id.
     * If the fields is not specified, all accessible fields as the default.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName     model name
     * @param id            data id
     * @param fields        field list to read
     * @param effectiveDate effective date of timeline data
     * @return data row
     */
    @GetMapping(value = "/readOne", params = { "id", "fields" })
    @Operation(summary = "readOne: Read one row", description = "Read one row by id.")
    @Parameters({
            @Parameter(name = "id", description = "Data ID to be read.", schema = @Schema(type = "string")),
            @Parameter(name = "fields", description = "A list of field names to be read. If not specified, it defaults to all visible fields."),
            @Parameter(name = "effectiveDate", description = "Effective date for timeline model.")
    })
    @DataMask
    public ApiResponse<Map<String, Object>> readOne(@PathVariable String modelName,
                                                    @RequestParam K id,
                                                    @RequestParam List<String> fields,
                                                    @RequestParam(required = false) LocalDate effectiveDate) {
        ContextHolder.getContext().setEffectiveDate(effectiveDate);
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.readOne(modelName, id, fields, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Read multiple rows by ids.
     * If the fields is not specified, all accessible fields as the default.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName     model name
     * @param ids           List of data ids
     * @param fields        Field list to read
     * @param effectiveDate effective date of timeline data
     * @return List<Map> of multiple data
     */
    @GetMapping(value = "/readList", params = { "ids", "fields" })
    @Operation(summary = "readList: Read multiple rows", description = "Read multiple rows by ids.")
    @Parameters({
            @Parameter(name = "ids", description = "Data IDs to be read."),
            @Parameter(name = "fields", description = "A list of field names to be read. If not specified, it defaults to all visible fields."),
            @Parameter(name = "effectiveDate", description = "Effective date for timeline model.")
    })
    @DataMask
    public ApiResponse<List<Map<String, Object>>> readList(@PathVariable String modelName,
                                                           @RequestParam List<K> ids,
                                                           @RequestParam List<String> fields,
                                                           @RequestParam(required = false) LocalDate effectiveDate) {
        ContextHolder.getContext().setEffectiveDate(effectiveDate);
        this.validateIds(ids);
        ids = IdUtils.formatIds(modelName, ids);
        return ApiResponse.success(modelService.readList(modelName, ids, fields, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Update one row by id.
     *
     * @param modelName model name
     * @param row       data row to be updated
     * @return true / Exception
     */
    @PostMapping(value = "/updateOne")
    @Operation(summary = "updateOne: Update one row", description = "Update one row by id. Return true on success.")
    @DataMask
    public ApiResponse<Boolean> updateOne(@PathVariable String modelName,
                                          @RequestBody Map<String, Object> row) {
        Assert.notNull(row.get("id"), "`id` cannot be null or missing when updating data!");
        IdUtils.formatMapId(modelName, row);
        return ApiResponse.success(modelService.updateOne(modelName, row));
    }

    /**
     * Update one row by id, and return the updated row fetched from the database, with the latest field values.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName model name
     * @param row       data row to be updated
     * @return map of updated row data with the latest field values
     */
    @PostMapping(value = "/updateOneAndReturn")
    @Operation(summary = "updateOneAndReturn: Update one row and return",
            description = "Update one row by id, and return the latest values from database.")
    @DataMask
    public ApiResponse<Map<String, Object>> updateOneAndReturn(@PathVariable String modelName,
                                                               @RequestBody Map<String, Object> row) {
        Assert.notEmpty(row, "The data to be updated cannot be empty!");
        Assert.notNull(row.get("id"), "`id` cannot be null or missing when updating data!");
        IdUtils.formatMapId(modelName, row);
        return ApiResponse.success(modelService.updateOneAndReturn(modelName, row, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Update multiple rows by ids. Each row in the list can have different fields.
     *
     * @param modelName model name
     * @param rows      data rows to be updated
     * @return true / Exception
     */
    @PostMapping(value = "/updateList")
    @Operation(summary = "updateList: Batch update", description = "Update multiple rows by id. Return true on success.")
    public ApiResponse<Boolean> updateList(@PathVariable String modelName,
                                           @RequestBody List<Map<String, Object>> rows) {
        Assert.notEmpty(rows, "The data to be updated cannot be empty!");
        this.validateBatchSize(rows.size());
        IdUtils.formatMapIds(modelName, rows);
        return ApiResponse.success(modelService.updateList(modelName, rows));
    }

    /**
     * Update multiple rows by ids.Each row in the list can have different fields.
     * And return the updated rows fetched from the database, with the latest field values.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName model name
     * @param rows      data rows to be updated
     * @return updated rows with the latest field values
     */
    @PostMapping(value = "/updateListAndReturn")
    @Operation(summary = "updateListAndReturn: Batch update and return",
            description = "Update multiple rows by id, and return the latest values from database.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> updateListAndReturn(@PathVariable String modelName,
                                                                      @RequestBody List<Map<String, Object>> rows) {
        Assert.notEmpty(rows, "The data to be updated cannot be empty!");
        this.validateBatchSize(rows.size());
        IdUtils.formatMapIds(modelName, rows);
        return ApiResponse.success(modelService.updateListAndReturn(modelName, rows, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Batch edit data based on the filters, according to the specified field values map.
     *
     * @param modelName model name
     * @param filters   filters, if not specified, all visible data of the current user will be updated.
     * @param value     field values to be updated
     * @return number of affected rows
     */
    @PostMapping(value = "/updateByFilter")
    @Operation(summary = "updateByFilter: Batch edit",
            description = "Update data according to the filters, within the current user's permission scope.")
    @Parameters({
            @Parameter(name = "filters", description = "Data filter to update."),
    })
    public ApiResponse<Integer> updateByFilter(@PathVariable String modelName,
                                               @RequestParam(required = false) Filters filters,
                                               @RequestBody Map<String, Object> value) {
        Assert.notEmpty(value, "The data to be updated cannot be empty!");
        Integer count = modelService.updateByFilter(modelName, filters, value);
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
    @PostMapping(value = "/deleteOne")
    @Operation(summary = "deleteOne: Delete one row",
            description = "Delete one row by id. All slices related to this `id` will be deleted for timeline model.")
    @Parameter(name = "id", description = "Data ID to be deleted")
    public ApiResponse<Boolean> deleteOne(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.deleteOne(modelName, id));
    }

    /**
     * Delete a slice of timeline model by `sliceId`, the primary key of timeline model.
     *
     * @param modelName model name
     * @param sliceId   data id
     * @return True / Exception
     */
    @PostMapping(value = "/deleteSlice")
    @Operation(summary = "deleteSlice: Delete one slice",
            description = "Delete a slice of the timeline model by `sliceId`.")
    @Parameter(name = "sliceId", description = "`sliceId` of the timeline slice data to delete.")
    public ApiResponse<Boolean> deleteSlice(@PathVariable String modelName, @RequestParam K sliceId) {
        sliceId = IdUtils.formatId(modelName, ModelConstant.SLICE_ID, sliceId);
        return ApiResponse.success(modelService.deleteSlice(modelName, sliceId));
    }

    /**
     * Delete multiple rows by ids.
     *
     * @param modelName model name
     * @param ids       data ids
     * @return True / Exception
     */
    @PostMapping(value = "/deleteList")
    @Operation(summary = "deleteList: Batch delete", description = "Delete multiple rows by ids.")
    @Parameter(name = "ids", description = "Data IDs to be deleted.")
    public ApiResponse<Boolean> deleteList(@PathVariable String modelName,
                                           @RequestParam(required = false) List<K> ids) {
        this.validateIds(ids);
        ids = IdUtils.formatIds(modelName, ids);
        return ApiResponse.success(modelService.deleteList(modelName, ids));
    }

    /**
     * Copy a single row based on id, and return the id of the new row.
     *
     * @param modelName model name
     * @param id        data source ID to be copied.
     * @return id of the new data
     */
    @PostMapping(value = "/copyOne")
    @Operation(summary = "copyOne: Copy one row",
            description = "Copy a single row based on id, and return the id of the new row.")
    @Parameter(name = "id", description = "Data source ID to be copied.")
    @DataMask
    public ApiResponse<K> copyOne(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.copyOne(modelName, id));
    }

    /**
     * Copy a single row based on id, and return the new row.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName model name
     * @param id        data source id
     * @return new row data
     */
    @PostMapping(value = "/copyOneAndReturn")
    @Operation(summary = "copyOneAndReturn: Copy one row and return",
            description = "Copy a single row based on id, and return the new row.")
    @Parameter(name = "id", description = "Data source ID to be copied.")
    @DataMask
    public ApiResponse<Map<String, Object>> copyOneAndReturn(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.copyOneAndReturn(modelName, id, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Copy multiple rows based on ids, and return the ids of the new rows.
     *
     * @param modelName model name
     * @param ids       data source ids
     * @return ids of the new data
     */
    @PostMapping(value = "/copyList")
    @Operation(summary = "copyList: Batch copy",
            description = "Copy multiple rows based on ids, and return the new data ids.")
    @Parameter(name = "ids", description = "Data source IDs to be copied.")
    @DataMask
    public ApiResponse<List<K>> copyList(@PathVariable String modelName, @RequestParam List<K> ids) {
        this.validateIds(ids);
        ids = IdUtils.formatIds(modelName, ids);
        return ApiResponse.success(modelService.copyList(modelName, ids));
    }

    /**
     * Copy multiple rows based on ids, and return the new rows.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     *
     * @param modelName model name
     * @param ids       source data ids
     * @return new rows data
     */
    @PostMapping(value = "/copyListAndReturn")
    @Operation(summary = "copyListAndReturn: Batch copy and return",
            description = "Copy multiple rows based on ids, and return the new rows.")
    @Parameter(name = "ids", description = "Data source IDs to be copied.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> copyListAndReturn(@PathVariable String modelName,
                                                                    @RequestParam List<K> ids) {
        this.validateIds(ids);
        ids = IdUtils.formatIds(modelName, ids);
        return ApiResponse.success(modelService.copyListAndReturn(modelName, ids, ConvertType.KEY_AND_DISPLAY));
    }

    /**
     * Copy a single row based on id, only return the copyable field values, without creating a new row.
     *
     * @param modelName model name
     * @param id        source data id
     * @return map of copyable field values
     */
    @GetMapping("/copyWithoutCreate")
    @Operation(summary = "copyWithoutCreate: Copy field values",
            description = "Copy one row by id, only return the copyable field values, without inserting into database.")
    @Parameter(name = "id", description = "Data source ID to be copied.")
    @DataMask
    public ApiResponse<Map<String, Object>> copyWithoutCreate(@PathVariable String modelName, @RequestParam K id) {
        id = IdUtils.formatId(modelName, id);
        return ApiResponse.success(modelService.copyWithoutCreate(modelName, id));
    }

    /**
     * Query data with pagination. The page size cannot exceed the MAX_BATCH_SIZE.
     * Set `convertType = KEY_AND_DISPLAY` to get the display value of expandable fields.
     * Support SUM, AVG, MIN, MAX, COUNT aggregation queries.
     *
     * @param modelName model name
     * @param aggQuery  aggregation query parameters
     * @return data list in the page
     */
    @PostMapping(value = "/searchPage")
    @Operation(summary = "searchPage: Paging query",
            description = "Paging aggregation query parameters, including fields, filters, orders, pageNumber, pageSize" +
                    "groupBy, aggFunctions, subQueries, etc. Use the backend default value when not specified.")
    @DataMask
    public ApiResponse<Page<Map<String, Object>>> searchPage(@PathVariable String modelName,
                                                             @RequestBody AggQuery aggQuery) {
        ContextHolder.getContext().setEffectiveDate(aggQuery.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(aggQuery.getFilters(), aggQuery.getOrders());
        flexQuery.setFields(aggQuery.getFields());
        flexQuery.setConvertType(ConvertType.KEY_AND_DISPLAY);
        flexQuery.setSummary(Boolean.TRUE.equals(aggQuery.getSummary()));
        flexQuery.setGroupBy(aggQuery.getGroupBy());
        // Set AggFunction parameters
        flexQuery.setAggFunctions(aggQuery.getAggFunctions());
        // Set SubQuery parameters
        flexQuery.expandSubQueries(aggQuery.getSubQueries());
        Page<Map<String, Object>> page = Page.of(aggQuery.getPageNumber(), aggQuery.getPageSize());
        return ApiResponse.success(modelService.searchPage(modelName, flexQuery, page));
    }

    /**
     * Query data list without pagination, the `limitSize` is default to DEFAULT_PAGE_SIZE.
     * The OneToOne/ManyToOne field value is the `displayName` of the related model.
     * Support SUM, AVG, MIN, MAX, COUNT aggregation queries.
     *
     * @param modelName model name
     * @param aggQuery  Aggregation query parameter
     * @return data list
     */
    @PostMapping(value = "/searchList")
    @Operation(summary = "searchList: Query data list", description = "Get the data list based on the specified" +
            "fields, filters, orders, limitSize, aggFunctions, and subQueries. Default limit to 50.")
    @DataMask
    public ApiResponse<List<Map<String, Object>>> searchList(@PathVariable String modelName,
                                                             @RequestBody AggQuery aggQuery) {
        ContextHolder.getContext().setEffectiveDate(aggQuery.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(aggQuery.getFilters(), aggQuery.getOrders());
        flexQuery.setFields(aggQuery.getFields());
        flexQuery.setConvertType(ConvertType.KEY_AND_DISPLAY);
        flexQuery.setGroupBy(aggQuery.getGroupBy());
        // Set AggFunction parameters
        flexQuery.setAggFunctions(aggQuery.getAggFunctions());
        // Set SubQuery parameters
        flexQuery.expandSubQueries(aggQuery.getSubQueries());
        // Default limitSize for searchList.
        Integer limitSize = aggQuery.getPageSize();
        limitSize = limitSize == null || limitSize < 1 ? BaseConstant.DEFAULT_PAGE_SIZE : limitSize;
        Assert.isTrue(limitSize <= BaseConstant.MAX_BATCH_SIZE,
                "API `searchList` cannot exceed the maximum limit of {0}.", BaseConstant.MAX_BATCH_SIZE);
        flexQuery.setLimitSize(limitSize);
        return ApiResponse.success(modelService.searchList(modelName, flexQuery));
    }

    /**
     * Simple aggregation query by `filters` and `aggFunctions`.
     * SUM, AVG, MIN, MAX, COUNT aggregation queries, such as `["SUM", "amount"]`, or `[["SUM", "amount"], [], ...]`.
     * Each key of the result is a camel case string concatenated with the functionName + fieldName, e.g., `sumAmount`.
     * Use the searchPage interface when grouping or paging is needed.
     *
     * @param modelName model name
     * @param simpleAggQuery Simple aggregation query parameters
     * @return Result map.
     */
    @PostMapping(value = "/searchSimpleAgg")
    @Operation(summary = "searchSimpleAgg: Simple aggregation query", description = """
            Pure SUM, AVG, MIN, MAX, COUNT aggregate query, like `["SUM", "amount"]` or `[["SUM", "amount"], [], ...]`,
            the return key is `sumAmount`.""")
    @DataMask
    public ApiResponse<Map<String, Object>> searchSimpleAgg(@PathVariable String modelName,
                                                            @RequestBody SimpleAggQuery simpleAggQuery) {
        ContextHolder.getContext().setEffectiveDate(simpleAggQuery.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(simpleAggQuery.getFilters());
        Assert.notTrue(AggFunctions.isEmpty(simpleAggQuery.getAggFunctions()), "`aggFunctions` cannot be null!");
        // Set AggFunction parameters
        flexQuery.setAggFunctions(simpleAggQuery.getAggFunctions());
        return ApiResponse.success(modelService.searchOne(modelName, flexQuery));
    }

    /**
     * Query the PivotTable, without pagination, the data is limited to no more than DEFAULT_BATCH_SIZE records.
     *
     * @param modelName model name
     * @param aggQuery  aggregation query parameters
     * @return PivotTable object
     */
    @PostMapping(value = "/searchPivot")
    @Operation(summary = "searchPivot: Search pivot table data ",
            description = "Get the pivot table data based on the specified fields, filters, orders, groupBy, splitBy.")
    @DataMask
    public ApiResponse<PivotTable> searchPivot(@PathVariable String modelName, @RequestBody AggQuery aggQuery) {
        ContextHolder.getContext().setEffectiveDate(aggQuery.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(aggQuery.getFilters(), aggQuery.getOrders());
        flexQuery.setFields(aggQuery.getFields());
        flexQuery.setConvertType(ConvertType.KEY_AND_DISPLAY);
        flexQuery.setGroupBy(aggQuery.getGroupBy());
        flexQuery.setSplitBy(aggQuery.getSplitBy());
        return ApiResponse.success(modelService.searchPivot(modelName, flexQuery));
    }

    /**
     * Count query. Support group counting by `groupBy` parameter, count the number of each group.
     * That is, the amount of data under each grouped field value. For example:
     * `groupBy=name,code,sequence & orders=sequence`, the actual sql statement is:
     * `SELECT name, code, sequence, count(*) AS count FROM table_name GROUP BY name, code, sequence ORDER BY sequence`
     *
     * @param filters filters
     * @param groupBy group by fields. Return the total count if not specified.
     * @param orders  orders of the grouped results, usually only one field is specified, such as sequence.
     * @param effectiveDate Effective date of the timeline.
     * @return Group counting results.
     */
    @GetMapping(value = "/count")
    @Operation(summary = "count: Count",
            description = "Returns a count or group counting based on the specified `filter`, `groupBy`, and `orders`.")
    @Parameters({
            @Parameter(name = "filters", description = "Filters for data to be counted."),
            @Parameter(name = "groupBy", description = "Fields for group counts, Return the total count if not specified."),
            @Parameter(name = "orders", description = "The field order of the grouped results"),
            @Parameter(name = "effectiveDate", description = "Effective date for timeline model.")
    })
    @DataMask
    public ApiResponse<Object> count(@PathVariable String modelName,
                                     @RequestParam(required = false) Filters filters,
                                     @RequestParam(required = false) List<String> groupBy,
                                     @RequestParam(required = false) Orders orders,
                                     @RequestParam(required = false) LocalDate effectiveDate) {
        ContextHolder.getContext().setEffectiveDate(effectiveDate);
        if (!CollectionUtils.isEmpty(groupBy)) {
            Assert.allNotBlank(groupBy, "`groupBy` cannot contain empty value: {0}", groupBy);
            FlexQuery flexQuery = new FlexQuery(filters, orders);
            flexQuery.setFields(new HashSet<>(groupBy));
            flexQuery.setGroupBy(groupBy);
            flexQuery.setConvertType(ConvertType.DEFAULT);
            return ApiResponse.success(modelService.searchList(modelName, flexQuery));
        } else {
            return ApiResponse.success(modelService.count(modelName, filters));
        }
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
    @Operation(summary = "getUnmaskedField: Gets unmasked field value",
            description = "Get the original value for masking field.")
    @Parameters({
            @Parameter(name = "id", required = true, description = "Data ID to be read"),
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
    @Operation(summary = "getUnmaskedFields: Gets multiple unmasked field values",
            description = "Get the original values for multiple masking fields.")
    @Parameters({
            @Parameter(name = "id", description = "Data ID to be read"),
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
}
