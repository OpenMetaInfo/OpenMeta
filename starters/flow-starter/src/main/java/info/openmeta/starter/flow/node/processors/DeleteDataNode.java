package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.DeleteDataParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.utils.FlowUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.Collection;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;

/**
 * Processor for DeleteData node.
 * Delete data based on the specified primary key or filter conditions.
 */
@Slf4j
@Component
public class DeleteDataNode implements NodeProcessor<DeleteDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.DELETE_DATA;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<DeleteDataParams> getParamsClass() {
        return DeleteDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, DeleteDataParams nodeParams) {
        Assert.notBlank(nodeParams.getModelName(),
                "The model name for Delete Node {0} cannot be empty!", flowNode.getName());
        Assert.notTrue(StringUtils.isBlank(nodeParams.getPkVariable()) && Filters.isEmpty(nodeParams.getFilters()),
                "In the parameter configuration of the Delete Node {0}, at least " +
                        "the primary key parameter or the filter condition for deleting data must be specified.",
                flowNode.getName());
    }

    /**
     * Execute the Delete Node processor.
     * Either the primary key or filters must be provided.
     * When both appear at the same time, merge the primary key and filters with AND.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, DeleteDataParams nodeParams, NodeContext nodeContext) {
        String pkVariable = nodeParams.getPkVariable();
        Filters deleteFilters = new Filters();
        if (StringTools.isVariable(pkVariable)) {
            // String variable parameter `#{}`: retrieve ids from the nodeContext
            Collection<?> ids = FlowUtils.getIdsFromPkVariable(flowNode, pkVariable, nodeContext);
            if (CollectionUtils.isEmpty(ids)) {
                return;
            }
            deleteFilters.in(ID, ids);
        }
        // Merge primary key list with filters to form the final delete conditions
        if (!Filters.isEmpty(nodeParams.getFilters())) {
            // Convert variables and calculation formulas in filters
            Filters clonedFilters = nodeParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(nodeParams.getModelName(), clonedFilters, nodeContext);
            deleteFilters.and(clonedFilters);
        }
        if (Filters.isEmpty(deleteFilters)) {
            return;
        }
        modelService.deleteByFilters(nodeParams.getModelName(), deleteFilters);
    }
}
