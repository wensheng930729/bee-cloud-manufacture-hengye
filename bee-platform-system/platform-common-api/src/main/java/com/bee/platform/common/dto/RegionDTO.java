package com.bee.platform.common.dto;

import com.bee.platform.common.entity.RegionInfo;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-06 13:48
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class RegionDTO implements Serializable {

    private static final long serialVersionUID = -1549088434706794258L;
    /**
     * 获取到区级信息
     */
    @ApiModelProperty("区级信息")
    private RegionInfo county;
    /**
     * 获取到市级信息
     */
    @ApiModelProperty("市级信息")
    private RegionInfo city;
    /**
     * 获取到省级信息
     */
    @ApiModelProperty("省级信息")
    private RegionInfo province;
}
