package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysModel;
import info.openmeta.starter.metadata.service.SysModelService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

/**
 * SysModel Model Service Implementation
 */
@Service
public class SysModelServiceImpl extends EntityServiceImpl<SysModel, Long> implements SysModelService {

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