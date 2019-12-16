package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("电价区间校验请求参数")
public class ConfigElectricityPriceUpdateCheckRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    /**
     * 生效日期
     */
    @ApiModelProperty("生效日期")
    private Date effectiveDate;
    /**
     * 失效日期
     */
    @ApiModelProperty("失效日期")
    private Date expirationDate;
    /**
     * 开始时间
     */
    @ApiModelProperty("开始时间")
    private Date startTime;
    /**
     * 结束时间
     */
    @ApiModelProperty("结束时间")
    private Date endTime;


}
