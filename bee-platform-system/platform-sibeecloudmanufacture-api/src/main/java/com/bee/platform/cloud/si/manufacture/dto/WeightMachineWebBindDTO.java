package com.bee.platform.cloud.si.manufacture.dto;

import com.bee.platform.cloud.si.manufacture.rq.CommonFileRq;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息保存入参
 * @Date 2019/9/23 19:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "地磅信息绑定物流出参")
public class WeightMachineWebBindDTO implements Serializable {

    @ApiModelProperty(value = "1 采购 2 销售")
    private Integer type;

    @ApiModelProperty(value = "磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "企业id")
    private Integer enterpriseId;

    @ApiModelProperty(value = "工厂id")
    private Integer factoryId;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "承运方")
    private String carrierName;

    @ApiModelProperty(value = "客户/供应商")
    private String custOrSupName;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "净重")
    private BigDecimal netWeight;

    @ApiModelProperty(value = "司磅员")
    private String weightMan;

    @ApiModelProperty(value = "司磅日期")
    private Date weighingTime;

}
