package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName LogisticsContractListContentDTO
 * @Description 物流列表内容
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("物流列表内容")
public class LogisticsContractListContentDTO {

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("合同类型  1采购 2销售")
    private Integer businessType;

    @ApiModelProperty("合同类型")
    private String businessTypeName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("合同数量")
    private BigDecimal quantity;

    @ApiModelProperty("产品单位")
    private String unitValue;

    @ApiModelProperty("起点")
    private String originAddress;

    @ApiModelProperty("终点")
    private String endAddress;

    @ApiModelProperty("创建日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date createTime;

}
