package info.openmeta.framework.orm.jdbc;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.utils.MapUtils;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * A tool class for filling in the audit fields of the data.
 */
public class AutofillFields {

    /**
     * Get the audit field values for inserting data.
     *
     * @param insertTime Insert time
     * @return Audit field values to be filled
     */
    private static Map<String, Object> getInsertAudit(LocalDateTime insertTime) {
        Long userId = ContextHolder.getContext().getUserId();
        String name = ContextHolder.getContext().getName();
        return MapUtils.strObj()
                .put(ModelConstant.CREATED_ID, userId)
                .put(ModelConstant.CREATED_BY, name)
                .put(ModelConstant.CREATED_TIME, insertTime)
                .put(ModelConstant.UPDATED_ID, userId)
                .put(ModelConstant.UPDATED_TIME, insertTime)
                .put(ModelConstant.UPDATED_BY, name)
                .build();
    }

    /**
     * Get the audit field values for updating data.
     *
     * @param updatedTime Update time
     * @return Audit field values to be filled
     */
    private static Map<String, Object> getUpdateAudit(LocalDateTime updatedTime) {
        Long userId = ContextHolder.getContext().getUserId();
        String name = ContextHolder.getContext().getName();
        return MapUtils.strObj()
                .put(ModelConstant.UPDATED_ID, userId)
                .put(ModelConstant.UPDATED_TIME, updatedTime)
                .put(ModelConstant.UPDATED_BY, name)
                .build();
    }

    /**
     * Batch fill in the audit fields when inserting.
     *
     * @param rows    List data
     * @param insertTime Insert time
     */
    public static void fillAuditFieldsForInsert(List<Map<String, Object>> rows, LocalDateTime insertTime) {
        rows.forEach(row -> row.putAll(getInsertAudit(insertTime)));
    }

    /**
     * Batch fill in the audit fields when updating.
     *
     * @param rows     List data
     * @param updatedTime Update time
     */
    public static void fillAuditFieldsForUpdate(List<Map<String, Object>> rows, LocalDateTime updatedTime) {
        rows.forEach(row -> row.putAll(getUpdateAudit(updatedTime)));
    }

    /**
     * Fill in the tenant ID when inserting data, using the tenant ID of the current user.
     *
     * @param rows List data
     */
    public static void fillTenantFieldForInsert(List<Map<String, Object>> rows) {
        Long tenantId = ContextHolder.getContext().getTenantId();
        rows.forEach(row -> row.put(ModelConstant.TENANT_ID, tenantId));
    }

}
