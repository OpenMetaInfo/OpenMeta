package info.openmeta.framework.orm.jdbc.database.builder;

import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

/**
 * OrderBy Builder
 * For paged queries:
 *      When `orders` in flexQuery is empty, using the `defaultOrder` configuration of model.
 *      When 'defaultOrder' is not configured, using the global default order `DEFAULT_PAGED_ORDER`.
 * For non-paged queries:
 *      Order according to the `orders` in flexQuery, or do not specify the sort when it is empty.
 */
public class OrderByBuilder extends BaseBuilder implements SqlClauseBuilder {

    private Page<?> page;

    public OrderByBuilder(SqlWrapper sqlWrapper, FlexQuery flexQuery) {
        super(sqlWrapper, flexQuery);
    }

    public <T> OrderByBuilder(SqlWrapper sqlWrapper, FlexQuery flexQuery, Page<T> page) {
        super(sqlWrapper, flexQuery);
        this.page = page;
    }

    public void build() {
        // Must be after groupBy processing
        handleOrderBy();
    }

    /**
     * Update sqlWrapper according to whether it is paged and the `orders` attribute of flexQuery
     */
    public void handleOrderBy() {
        Orders orders = flexQuery.getOrders();
        if (orders == null) {
            // When `orders` in flexQuery is empty, using the `defaultOrder` configuration of model.
            String defaultOrder = ModelManager.getModel(mainModelName).getDefaultOrder();
            if (StringUtils.isNotBlank(defaultOrder)) {
                orders = Orders.of(defaultOrder);
            } else if (page != null && !flexQuery.isAggregate()) {
                // In page query, if the order is not specified, and it is not an aggregate query, use the default order.
                orders = Orders.of(ModelConstant.DEFAULT_PAGED_ORDER);
            }
        }
        if (orders != null) {
            for (List<String> order : orders.getOrderList()) {
                String aliasField = this.parseLogicField(order.get(0));
                sqlWrapper.orderBy(aliasField, order.get(1));
            }
            // For stable order paging queries, if there is no `id` in the orders parameter,
            // automatically add `t.id ASC` at the end of the order condition,
            // to ensure that different page data is as non-repetitive as possible.
            if (page != null && page.isCursorPage() && !orders.getFields().contains(ModelConstant.ID)) {
                sqlWrapper.orderBy(SqlWrapper.MAIN_TABLE_ALIAS + "." + ModelConstant.ID, Orders.ASC);
            }
        }
    }

}
