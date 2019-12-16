package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Description 过磅单数据信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/11/12 09:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "过磅单数据信息")
public class WeighingListDTO implements Serializable {

    @ApiModelProperty(value = "磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "采购合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty(value = "客商")
    private String merchants;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "承运方")
    private String carrierName;

    @ApiModelProperty(value = "毛重")
    private BigDecimal grossWeight;

    @ApiModelProperty(value = "皮重")
    private BigDecimal tareWeight;

    @ApiModelProperty(value = "扣重")
    private BigDecimal deductWeight;

    @ApiModelProperty(value = "净重")
    private BigDecimal netWeight;

    @ApiModelProperty(value = "司磅员")
    private String weighingMan;

    @ApiModelProperty(value = "称重时间")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date weighingTime;

    @ApiModelProperty(value = "称重类型")
    private Integer weighingType;

}
