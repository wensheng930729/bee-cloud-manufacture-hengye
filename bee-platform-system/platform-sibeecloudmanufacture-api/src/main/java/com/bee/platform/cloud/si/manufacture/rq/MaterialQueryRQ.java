package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批列表查询参数")
public class MaterialQueryRQ implements Serializable {
    private static final long serialVersionUID = 9053655546853376899L;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("成品名称")
    private String finishProductName;

    @ApiModelProperty("企业id")
    private Integer companyId;

    @ApiModelProperty("工厂id")
    private Integer factoryId;

    @ApiModelProperty("状态：1启用，0停用")
    private Integer active;

    @ApiModelProperty("创建时间开始时间")
    private String startTime;

    @ApiModelProperty("创建时间结束时间")
    private String endTime;
}
