package info.openmeta.starter.designer.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.designer.dto.ModelCodeDTO;
import info.openmeta.starter.designer.entity.DesignModel;

/**
 * DesignModel Model Service Interface
 */
public interface DesignModelService extends EntityService<DesignModel, Long> {

    /**
     * Preview the DDL SQL of model, including table creation and index creation
     *
     * @param id Model ID
     * @return Model DDL SQL
     */
    String previewDDL(Long id);

    /**
     * Preview the current model code, including Java class code for Entity, Service, ServiceImpl, and Controller
     *
     * @param id Model ID
     * @return Code text of the current model
     */
    ModelCodeDTO previewCode(Long id);

}