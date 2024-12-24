package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.UpdateDataParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.utils.FlowUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;

/**
 * Processor for UpdateData node.
 * Update data based on the specified model, primary key or filters, and row template.
 */
@Component
public class UpdateDataNode implements NodeProcessor<UpdateDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.UPDATE_DATA;
    }

    @Override
    public Class<UpdateDataParams> getParamsClass() {
        return UpdateDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, UpdateDataParams nodeParams) {
        Assert.notEmpty(nodeParams.getRowTemplate(),
                "The model name parameter for Update Node {0} cannot be blank!", flowNode.getName());
        Assert.notTrue(StringUtils.isBlank(nodeParams.getPkVariable()) && Filters.isEmpty(nodeParams.getFilters()),
                """
                The parameter configuration for UpdateDataAction {0} must specify at least
                the primary key parameter or the update data filtering conditions.
                """, flowNode.getName());
    }

    /**
     * Execute the UpdateDataNode processor.
     * The value supports constants, variables, and calculation formulas,
     * where variables are represented by `#{}` and calculation formulas are represented by `${}`.
     * Example:
     * <p>
     * {
     *     "modelName": "SysModel",
     *     "pkVariable": "#{deptId}",
     *     "filters": ["code", "=", "#{deptCode}"],
     *     "rowTemplate":  {
     *         "parentId": "#{parentId}",
     *         "name": "#{deptName}",
     *         "ownId": "#{ownId}"
     *     }
     * }
     * </p>
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, UpdateDataParams nodeParams, NodeContext nodeContext) {
        String pkVariable = nodeParams.getPkVariable();
        Filters updateFilters = new Filters();
        if (StringTools.isVariable(pkVariable)) {
            // String variable parameter `#{}`: retrieve ids from the nodeContext
            Collection<?> ids = FlowUtils.getIdsFromPkVariable(flowNode, pkVariable, nodeContext);
            if (CollectionUtils.isEmpty(ids)) {
                return;
            }
            updateFilters.in(ID, ids);
        }
        // Merge primary key list with filters to form the final delete conditions
        if (!Filters.isEmpty(nodeParams.getFilters())) {
            // Convert variables and calculation formulas in filters
            Filters clonedFilters = nodeParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(nodeParams.getModelName(), clonedFilters, nodeContext);
            updateFilters.and(clonedFilters);
        }
        if (Filters.isEmpty(updateFilters)) {
            return;
        }
        // Generate updated data based on the data template
        Map<String, Object> rowMap = FlowUtils.resolveRowTemplate(nodeParams.getModelName(), nodeParams.getRowTemplate(), nodeContext);
        modelService.updateByFilter(nodeParams.getModelName(), updateFilters, rowMap);
    }

}
