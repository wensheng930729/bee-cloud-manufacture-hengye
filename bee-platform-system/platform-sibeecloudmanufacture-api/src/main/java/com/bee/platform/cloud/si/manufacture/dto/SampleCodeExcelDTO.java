package com.bee.platform.cloud.si.manufacture.dto;

import cn.afterturn.easypoi.excel.annotation.Excel;
import cn.afterturn.easypoi.excel.annotation.ExcelTarget;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: SampleCodeExcelDTO
 * @Description: 样品编码导出DTO
 * @Author: liliang
 * @Date: 2019/9/30
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ExcelTarget("SampleCodeExcelDTO")
public class SampleCodeExcelDTO {

    @Excel(name = "code")
    private String code;


}
