package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName BuyContractSettlePopupCarDTO
 * @Description 合同结算详情弹窗车次信息
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同结算详情弹窗车次信息")
public class BuyContractSettlePopupCarDTO {

    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("车牌号")
    private String trainNumber;

    @ApiModelProperty("净重")
    private BigDecimal netWeight;

    @ApiModelProperty("折扣单价 没有价格-是否折价：否")
    private BigDecimal discountUnitPrice;

    @ApiModelProperty("司磅日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date weighingTime;
}
