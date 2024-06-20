package info.openmeta.framework.web.controller;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.base.utils.Assert;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;

/**
 * The base controller of entity.
 */
public abstract class EntityController<Service extends EntityService<T, K>, T extends BaseModel, K extends Serializable> {

    @Autowired(required = false)
    protected Service service;

    /**
     * The size of operation data in a single API call cannot exceed the MAX_BATCH_SIZE.
     *
     * @param size data size
     */
    protected void validateBatchSize(int size) {
        Assert.isTrue(size <= BaseConstant.MAX_BATCH_SIZE,
                "The size of operation data cannot exceed the maximum {0} limit.", BaseConstant.MAX_BATCH_SIZE);
    }
}
