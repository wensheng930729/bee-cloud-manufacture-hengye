package com.bee.platform.common.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dao.mapper.DictionaryMapper;
import com.bee.platform.common.entity.Dictionary;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.DictionaryService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 字典表 服务实现类
 * </p>
 *
 * @author qhwang123
 * @since 2019-03-21
 */
@Service
@Slf4j
public class DictionaryServiceImpl extends ServiceImpl<DictionaryMapper, Dictionary> implements DictionaryService {

    @Autowired
    private DictionaryMapper dictionaryMapper;

    /**
     * 根据字典类型查询字典信息列表
     * @param type 字典类型
     */
    @Override
    public ResponseResult getDictionaryByType(Integer type) {
        try {
            if (type != null) {
                List<Dictionary> dictionaryList = dictionaryMapper.selectList(new EntityWrapper<Dictionary>().eq("type", type));
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dictionaryList);
            } else {
                log.error("字典类型为空！");
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
            }
        } catch (Exception e){
            log.error("查询失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

}
