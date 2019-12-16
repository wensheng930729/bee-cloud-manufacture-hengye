package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 电价管理
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "电价管理返回DTO")
@JsonInclude
public class ConfigElectricityPriceDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 所属企业id
     */
    @ApiModelProperty("所属企业id")
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @ApiModelProperty("工厂id")
    private Integer factoryId;
    /**
     * 用电类型(0炉变电 1动力电)
     */
    @ApiModelProperty("用电类型")
    private Integer electricityType;
    /**
     * 生效日期
     */
    @ApiModelProperty("生效日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date effectiveDate;
    /**
     * 失效日期
     */
    @ApiModelProperty("失效日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date expirationDate;
    /**
     * 开始时间
     */
    @ApiModelProperty("开始时间")
    @JsonFormat(pattern = "HH:mm")
    private Date startTime;
    /**
     * 结束时间
     */
    @ApiModelProperty("结束时间")
    @JsonFormat(pattern = "HH:mm")
    private Date endTime;
    /**
     * 电价
     */
    @ApiModelProperty("电价")
    private BigDecimal price;





}
