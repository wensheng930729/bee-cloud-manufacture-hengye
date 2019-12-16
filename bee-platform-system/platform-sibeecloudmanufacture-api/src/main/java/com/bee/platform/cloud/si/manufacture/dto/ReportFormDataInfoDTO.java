package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;
import java.util.Map;

/**
 * @ClassName ReportFormDataInfoDTO
 * @Description 报表返回数据DTO
 * @author qhwang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("报表返回数据DTO")
public class ReportFormDataInfoDTO {

    @ApiModelProperty("标题信息")
    private List<ReportTitleFieldsDTO> fields;

    @ApiModelProperty("数据集合")
    private List<Map<String, Object>> dataList;

}
