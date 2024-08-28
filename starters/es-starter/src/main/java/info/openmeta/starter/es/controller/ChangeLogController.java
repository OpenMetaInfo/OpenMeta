package info.openmeta.starter.es.controller;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.SystemRole;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.RequireRole;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.domain.QueryParams;
import info.openmeta.framework.orm.meta.MetaModel;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.es.service.ChangeLogService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * ChangeLog Controller
 */
@Tag(name = "ChangeLog")
@RestController
@RequestMapping("/ChangeLog")
public class ChangeLogController {

    @Autowired
    private ChangeLogService changeLogService;

    /**
     * Initialize the page object and context parameters
     *
     * @param pageNumber current page number
     * @param pageSize single page quantity
     * @param dataMask whether to desensitize, default is true, that is, data desensitization
     * @return page object
     */
    private Page<ChangeLog> initPageAndContext(Integer pageNumber, Integer pageSize, Boolean dataMask) {
        Page<ChangeLog> page = Page.of(pageNumber, pageSize);
        boolean isDataMask = !Boolean.FALSE.equals(dataMask);
        ContextHolder.getContext().setDataMask(isDataMask);
        return page;
    }

    /**
     * Get the change log by the primary key of the business data model.
     *
     * @param modelName model name
     * @param id primary key id
     * @param pageNumber current page number, default is 1
     * @param pageSize single page quantity, default is 50
     * @param order sort rule based on change time, default is reverse order, only support DESC, ASC string
     * @param includeCreation whether to include data at creation time, default is false, that is, not included
     * @param dataMask whether to desensitize, default is true, that is, data desensitization
     * @return a page of change log list
     */
    @Operation(description = "Read the data change records according to the id, " +
            "and return the page of the data change record, default in reverse order by change time.")
    @GetMapping("/getChangeLog")
    @Parameters({
            @Parameter(name = "modelName", description = "Model name"),
            @Parameter(name = "id", description = "Primary key id"),
            @Parameter(name = "pageNumber", description = "Current page number, default 1"),
            @Parameter(name = "pageSize", description = "Single page quantity, default 50"),
            @Parameter(name = "order", description = "DESC or ASC sort rule based on changeTime, default is DESC."),
            @Parameter(name = "includeCreation", description = "Whether to include data at creation time, default is false, that is, not included."),
            @Parameter(name = "dataMask", description = "Whether to desensitize, default is true, that is, data desensitization.")
    })
    public ApiResponse<Page<ChangeLog>> getChangeLog(@RequestParam String modelName,
                                                     @RequestParam Long id,
                                                     @RequestParam(required = false) Integer pageNumber,
                                                     @RequestParam(required = false) Integer pageSize,
                                                     @RequestParam(required = false) String order,
                                                     @RequestParam(required = false) Boolean includeCreation,
                                                     @RequestParam(required = false) Boolean dataMask) {
        MetaModel metaModel = ModelManager.getModel(modelName);
        Assert.notTrue(metaModel.isTimeline(),
                "The timeline model can only call the API to get the slice change log: getSliceChangeLog");
        Page<ChangeLog> page = this.initPageAndContext(pageNumber, pageSize, dataMask);
        order = Orders.ASC.equals(StringUtils.upperCase(order)) ? Orders.ASC : Orders.DESC;

        page = changeLogService.getChangeLog(modelName, id, page, order, Boolean.TRUE.equals(includeCreation));
        return ApiResponse.success(page);
    }

    /**
     * Get the change log by the primary key `sliceId` of the timeline model.
     *
     * @param modelName timeline model name
     * @param sliceId primary key id of the timeline model
     * @param pageNumber current page number, default is 1
     * @param pageSize single page quantity, default is 50
     * @param order sort rule based on change time, default is reverse order, only support DESC, ASC string
     * @param getCreate whether to include data at creation time, default is false, that is, not included
     * @param dataMask whether to desensitize, default is true, that is, data desensitization
     * @return a page of change log list
     */
    @Operation(description = "Read the timeline slice change log page according to sliceId, " +
            "default in reverse order by change time.")
    @GetMapping("/getSliceChangeLog")
    @Parameters({
            @Parameter(name = "modelName", description = "Timeline model name"),
            @Parameter(name = "sliceId", description = "Primary key id of the timeline model"),
            @Parameter(name = "pageNumber", description = "Current page number, default 1"),
            @Parameter(name = "pageSize", description = "Single page quantity, default 50"),
            @Parameter(name = "order", description = "DESC or ASC sort rule based on changeTime, default is DESC."),
            @Parameter(name = "getCreate", description = "Whether to include data at creation time, default is false, that is, not included."),
            @Parameter(name = "dataMask", description = "Whether to desensitize, default is true, that is, data desensitization.")
    })
    public ApiResponse<Page<ChangeLog>> getSliceChangeLog(@RequestParam String modelName,
                                                          @RequestParam Long sliceId,
                                                          @RequestParam(required = false) Integer pageNumber,
                                                          @RequestParam(required = false) Integer pageSize,
                                                          @RequestParam(required = false) String order,
                                                          @RequestParam(required = false) Boolean getCreate,
                                                          @RequestParam(required = false) Boolean dataMask) {
        MetaModel metaModel = ModelManager.getModel(modelName);
        Assert.isTrue(metaModel.isTimeline(),
                "Can only get the slice change log of the timeline model, {0} model is not a timeline model",
                modelName);
        Page<ChangeLog> page = this.initPageAndContext(pageNumber, pageSize, dataMask);
        order = Orders.ASC.equals(StringUtils.upperCase(order)) ? Orders.ASC : Orders.DESC;

        page =changeLogService.getSliceChangeLog(modelName, sliceId, page, order, Boolean.TRUE.equals(getCreate));
        return ApiResponse.success(page);
    }

    /**
     * Search the change log page by the model name and query parameters.
     *
     * @param modelName model name
     * @param queryParams query parameters
     * @return a page of change log list
     */
    @Operation(description = """
            Return paginated data based on the specified filters, sorting conditions, page number, page size.
            If not provided, using the backend default values.""")
    @PostMapping(value = "/searchPageOfModel")
    @Parameter(name = "modelName", description = "Model name")
    @RequireRole(SystemRole.SYSTEM_ROLE_ADMIN)
    public ApiResponse<Page<ChangeLog>> searchPageByModel(@RequestParam String modelName, @RequestBody QueryParams queryParams) {
        ContextHolder.getContext().setEffectiveDate(queryParams.getEffectiveDate());
        Page<ChangeLog> page = Page.of(queryParams.getPageNumber(), queryParams.getPageSize());
        return ApiResponse.success(changeLogService.searchPageByModel(modelName, queryParams.getFilters(), queryParams.getOrders(), page, true));
    }

    /**
     * Search the change log page by the query parameters.
     *
     * @param queryParams query parameters
     * @return a page of change log list
     */
    @Operation(description = """
            Return paginated data based on the specified filters, sorting conditions, page number, page size.
            If not provided, using the backend default values.""")
    @PostMapping(value = "/searchPage")
    @RequireRole(SystemRole.SYSTEM_ROLE_ADMIN)
    public ApiResponse<Page<ChangeLog>> searchPage(@RequestBody QueryParams queryParams) {
        Page<ChangeLog> page = Page.of(queryParams.getPageNumber(), queryParams.getPageSize());
        return ApiResponse.success(changeLogService.searchPage(queryParams.getFilters(), queryParams.getOrders(), page, true));
    }
}
