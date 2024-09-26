package info.openmeta.starter.file.excel.handler;

import com.alibaba.excel.write.handler.CellWriteHandler;
import com.alibaba.excel.write.handler.context.CellWriteHandlerContext;
import com.alibaba.excel.write.metadata.style.WriteCellStyle;
import com.alibaba.excel.write.metadata.style.WriteFont;
import org.apache.poi.ss.usermodel.*;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * 表头自定义样式
 */
public class CustomHeadStyleHandler implements CellWriteHandler {

    // 动态必填字段的列表（传递进来）
    private final List<String> fieldRequiredList;

    public CustomHeadStyleHandler(List<String> fieldRequiredList) {
        this.fieldRequiredList = fieldRequiredList;
    }

    /**
     * 必填字段用红色字体
     */
    @Override
    public void afterCellDispose(CellWriteHandlerContext context) {
        if (!CollectionUtils.isEmpty(fieldRequiredList) && context.getHead()) {
            if (fieldRequiredList.contains(context.getHeadData().getHeadNameList().getFirst())) {
                WriteCellStyle writeCellStyle = context.getFirstCellData().getOrCreateStyle();
                // 新建字体合并单元格现有的字体样式
                WriteFont customFont = new WriteFont();
                WriteFont.merge(writeCellStyle.getWriteFont(), customFont);
                customFont.setColor(IndexedColors.RED.getIndex());
                writeCellStyle.setWriteFont(customFont);
            }
        }
    }

}
