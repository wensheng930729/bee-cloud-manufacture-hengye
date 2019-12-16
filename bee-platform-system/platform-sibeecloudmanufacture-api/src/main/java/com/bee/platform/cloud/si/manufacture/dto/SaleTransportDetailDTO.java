package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName SaleTransportDetailDTO
 * @Description 批次运输段中的车次信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/27 9:19
 */
@Data
@Accessors(chain = true)
@ApiModel("运输段中的车次信息")
public class SaleTransportDetailDTO {

    @ApiModelProperty("批次ID")
    private String batchId;
    
    @ApiModelProperty("批次名称")
    private String batchName;

    @ApiModelProperty("采购合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("运输段id")
    private String transportSectionId;

    @ApiModelProperty("承运方运输ID")
    private String carrierTransportId;

    @ApiModelProperty("承运方运输车次ID")
    private String carrierTransportDetailId;

    @ApiModelProperty("运输方式(1-汽车 2-轮船 3-火车)")
    private Integer transportMode;

    @ApiModelProperty("运输方式描述(1-汽车 2-轮船 3-火车)")
    private String transportModeName;

    @ApiModelProperty("车牌号/船号/火车车次")
    private String trainNumber;

    @ApiModelProperty("司机/船长")
    private String driver;

    @ApiModelProperty("司机/船长联系方式")
    private String contact;

    @ApiModelProperty("载货重量")
    private BigDecimal cargoWeight;

    @ApiModelProperty("到货确认化验结果(0-不合格 1-合格)")
    private Integer assayResult;

}
