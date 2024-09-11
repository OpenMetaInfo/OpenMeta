package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.orm.service.ModelService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
public class ImportDataHandler {

    @Autowired
    private ModelService<?> modelService;

    public void importData(List<Map<String, Object>> dataList) {
        // convert data
    }

}
