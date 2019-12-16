package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName ReportTitleFieldsDTO
 * @Description 报表标题字段信息
 * @author qhwang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("报表标题字段信息")
public class ReportTitleFieldsDTO {

    @ApiModelProperty("字段key")
    private String dataIndex;

    @ApiModelProperty("字段名称")
    private String title;

    @ApiModelProperty("子标题信息")
    private List<ReportTitleFieldsDTO> children;

}
