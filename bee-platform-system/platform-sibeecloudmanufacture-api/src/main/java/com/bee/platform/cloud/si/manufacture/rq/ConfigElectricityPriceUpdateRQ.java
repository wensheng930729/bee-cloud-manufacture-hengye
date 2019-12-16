package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
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
@ApiModel(value = "修改电价请求参数")
public class ConfigElectricityPriceUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;
    /**
     * 用电类型(0炉变电 1动力电)
     */
    @ApiModelProperty("用电类型(0炉变电 1动力电)")
    @NotNull(message = "用电类型不能为空")
    private Integer electricityType;
    /**
     * 生效日期
     */
    @ApiModelProperty("生效日期")
    @NotNull(message = "生效日期不能为空")
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
    @NotNull(message = "开始时间不能为空")
    @JsonFormat(pattern = "HH:mm")
    private Date startTime;
    /**
     * 结束时间
     */
    @ApiModelProperty("结束时间")
    @NotNull(message = "结束时间不能为空")
    @JsonFormat(pattern = "HH:mm")
    private Date endTime;
    /**
     * 电价
     */
    @ApiModelProperty("电价")
    @NotNull(message = "电价不能为空")
    private BigDecimal price;





}
