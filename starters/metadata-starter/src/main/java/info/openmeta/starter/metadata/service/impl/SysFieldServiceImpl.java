package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysField;
import info.openmeta.starter.metadata.service.SysFieldService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

/**
 * SysField Model Service Implementation
 */
@Service
public class SysFieldServiceImpl extends EntityServiceImpl<SysField, Long> implements SysFieldService {

    @Autowired
    @Lazy
    private ModelManager modelManager;

    /**
     * Reload the memory cache of model metadata.
     */
    public void reloadModelManager() {
        modelManager.init();
    }
}