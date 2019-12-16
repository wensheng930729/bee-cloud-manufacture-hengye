package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

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
@ApiModel(value = "地磅数据信息保存入参")
public class WeightMachineRq implements Serializable {

    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "企业id")
    private Integer enterpriseId;

    @ApiModelProperty(value = "工厂id")
    private Integer factoryId;

    @ApiModelProperty(value = "采购合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty(value = "上一段运输单位号码")
    private String lastTransportNum;

    @ApiModelProperty(value = "批次ID")
    private String batchId;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "司机")
    private String driver;

    @ApiModelProperty(value = "联系方式")
    private String contact;

    @ApiModelProperty(value = "设备id")
    private String deviceId;

    @ApiModelProperty(value = "承运方")
    private String carrierName;

    @ApiModelProperty(value = "发货单位")
    private String deliveryCompany;

    @ApiModelProperty(value = "收货单位")
    private String receivingCompany;

    @ApiModelProperty(value = "产品Id")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "载货重量")
    private BigDecimal cargoWeight;

    @ApiModelProperty(value = "称重时间")
    private Date weighingTime;

    @ApiModelProperty(value = "进厂重量")
    private BigDecimal inFactoryWeight;

    @ApiModelProperty(value = "出厂重量")
    private BigDecimal outFactoryWeight;

    @ApiModelProperty(value = "进厂重量是否手动录入 0 自动 1手动 2 手动（设备异常）")
    private Integer inFactoryWeightByManual;

    @ApiModelProperty(value = "出厂重量是否手动录入 0 自动 1手动 2 手动（设备异常）")
    private Integer outFactoryWeightByManual;

    @ApiModelProperty(value = "净重")
    private BigDecimal netWeight;

    @ApiModelProperty(value = "实际到厂时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm")
    private Date arrivalTime;

    @ApiModelProperty(value = "实际出厂时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm")
    private Date outTime;

    @ApiModelProperty(value = "附件信息")
    private List<CommonFileRq> files;

    @ApiModelProperty(value = "承运方id")
    private Integer carrierId;

    @ApiModelProperty(value = "发货单位id")
    private Integer deliveryCompanyId;

    @ApiModelProperty(value = "收货单位id")
    private Integer receivingCompanyId;

    @ApiModelProperty(value = "备注")
    private String remark;
}
