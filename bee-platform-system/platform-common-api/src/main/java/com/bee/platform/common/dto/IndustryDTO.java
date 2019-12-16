package com.bee.platform.common.dto;

import com.bee.platform.common.entity.IndustryInfo;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName IndustryDTO
 * @Description 功能描述
 * @Date 2019/4/24 16:33
 **/
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class IndustryDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 获取到父级行业信息
     */
    private IndustryInfo parentIndustry;

    /**
     * 获取到子级行业信息
     */
    private IndustryInfo childIndustry;

}
