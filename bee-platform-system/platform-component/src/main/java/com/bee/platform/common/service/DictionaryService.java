package com.bee.platform.common.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.Dictionary;
import com.bee.platform.common.entity.ResponseResult;

/**
 * <p>
 * 字典表 服务类
 * </p>
 *
 * @author qhwang123
 * @since 2019-03-21
 */
public interface DictionaryService extends IService<Dictionary> {

    /**
     * 根据字典类型查询字典信息列表
     * @param type 字典类型
     */
    public ResponseResult getDictionaryByType(Integer type);

}
