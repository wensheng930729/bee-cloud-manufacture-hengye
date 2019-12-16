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
 * <p>
 * 电价管理
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "电价管理返回DTO")
public class ConfigElectricityPriceCheckDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;

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






}
