package info.openmeta.starter.designer.service.impl;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.SubQueries;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.ddl.DDLFactory;
import info.openmeta.starter.designer.dto.ModelCodeDTO;
import info.openmeta.starter.designer.entity.DesignApp;
import info.openmeta.starter.designer.entity.DesignModel;
import info.openmeta.starter.designer.generator.CodeGenerator;
import info.openmeta.starter.designer.service.DesignAppService;
import info.openmeta.starter.designer.service.DesignModelService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;

/**
 * DesignModel Model Service Implementation
 */
@Service
public class DesignModelServiceImpl extends EntityServiceImpl<DesignModel, Long> implements DesignModelService {

    @Autowired
    private DesignAppService appService;

    /**
     * Preview the DDL SQL of model, including table creation and index creation
     *
     * @param id Model ID
     * @return Model DDL SQL
     */
    @Override
    public String previewDDL(Long id) {
        SubQueries subQueries = new SubQueries().expand(DesignModel::getModelFields)
                .expand(DesignModel::getModelIndexes);
        DesignModel designModel = this.readOne(id, subQueries);
        Assert.notNull(designModel, "The designModel id {0} does not exist!", id);
        StringBuilder ddl = new StringBuilder("-- Create table for model: ").append(designModel.getLabelName()).append("\n");
        ddl.append(DDLFactory.getInstance().createTableDDL(designModel)).append("\n");
        if (CollectionUtils.isEmpty(designModel.getModelIndexes())) {
            return ddl.toString();
        }
        ddl.append("-- Table index:").append("\n");
        ddl.append(DDLFactory.getInstance().alterTableIndexDDL(new ArrayList<>(), designModel.getModelIndexes())).append("\n");
        return ddl.toString();
    }

    /**
     * Preview the current model code, including Java class code for Entity, Service, ServiceImpl, and Controller
     *
     * @param id Model ID
     * @return Code text of the current model
     */
    @Override
    public ModelCodeDTO previewCode(Long id) {
        DesignModel designModel = this.readOne(id);
        Assert.notNull(designModel, "The designModel id {0} does not exist!", id);
        Assert.notNull(designModel.getAppId(), "The appId of the model cannot be null!");
        String packageName = appService.readField(designModel.getAppId(), DesignApp::getPackageName);
        Assert.notBlank(packageName, "To generate code, the packageName field of the App cannot be blank!");
        return CodeGenerator.generateModelCode(packageName, designModel);
    }

}