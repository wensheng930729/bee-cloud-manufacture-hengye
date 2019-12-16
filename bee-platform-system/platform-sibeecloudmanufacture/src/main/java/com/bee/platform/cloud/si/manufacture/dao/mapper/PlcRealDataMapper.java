package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.PlcRealData;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * plc硬件通过mqtt传输的实时数据 Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-10-10
 */
public interface PlcRealDataMapper extends BaseMapper<PlcRealData> {
    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 10:13 2019/10/12
     * @param list : 目标数据
     * @return: void
     */
    void insertAll(@Param("data") List<PlcRealData> list);
    /**
     * @notes: 获得plc最近一次的数据
     * @Author: junyang.li
     * @Date: 10:51 2019/10/12
     * @param plcId : plcid
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcRealData>
     */
    List<PlcRealData> getLastData(@Param("plcId") int plcId);
    /**
     * @notes: 通过漏斗查询最新数据
     * @Author: junyang.li
     * @Date: 22:00 2019/10/18
     * @param set : 漏斗集合
     * @param size : 查询条数
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.PlcRealData>
     */
    List<PlcRealData> getNewData(@Param("data") Set<String> set,@Param("size") int size) ;
}
