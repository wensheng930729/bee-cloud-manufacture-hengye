package com.bee.platform.cloud.si.manufacture.config.handler.mybatis;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedJdbcTypes;
import org.apache.ibatis.type.MappedTypes;

import com.alibaba.druid.support.json.JSONParser;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;

import lombok.extern.slf4j.Slf4j;

/**
 * @ClassName MySqlJsonObjectHandler
 * @Description  将json与对象的互相转换
 * @author zhigang.zhou
 * @Date 2019年09月16日 下午13:16:35
 * @version 1.0.0
 */
@Slf4j
@MappedTypes(Object.class)
@MappedJdbcTypes(JdbcType.VARCHAR)
public class MySqlJsonObjectHandler extends BaseTypeHandler<Object> {

	/**
	 * 设置非空参数
	 * 
	 * @param ps
	 * @param i
	 * @param parameter
	 * @param jdbcType
	 * @throws SQLException
	 */
	@Override
	public void setNonNullParameter(PreparedStatement ps, int i, Object parameter, JdbcType jdbcType)
			throws SQLException {
		ps.setString(i, JSONObject.toJSONString(parameter));
	}

	/**
	 * 根据列名，获取可以为空的结果
	 * 
	 * @param rs
	 * @param columnName
	 * @return
	 * @throws SQLException
	 */
	@Override
	public Object getNullableResult(ResultSet rs, String columnName) throws SQLException {
		String sqlJson = rs.getString(columnName);
		if (null != sqlJson) {
			Object obj = new JSONParser(sqlJson).parse();
			if (obj instanceof JSONObject) {
			    JSONObject jsonObject = (JSONObject) obj;
			    return jsonObject;
			} else if(obj instanceof JSONArray){
			    JSONArray jsonArray = (JSONArray) obj;
			    return jsonArray;
			}else {
				try {
					//log.info("getNullableResult(ResultSet rs, int columnIndex)=>JSONObject.parseObject");
					return JSONObject.parseObject(sqlJson);
				}catch (Exception e) {
					try {
						log.info("getNullableResult(ResultSet rs, int columnIndex)=>JSONArray.parseObject");
						return JSONArray.parseObject(sqlJson,List.class);
					} catch (Exception e1) {
						//log.info("getNullableResult(ResultSet rs, int columnIndex)=>return sqlJson");
						return sqlJson;
					}
				}
				
			}
		}
		return null;
	}

	/**
	 * 根据列索引，获取可以为空的结果
	 * 
	 * @param rs
	 * @param columnIndex
	 * @return
	 * @throws SQLException
	 */
	@Override
	public Object getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
		String sqlJson = rs.getString(columnIndex);
		if (null != sqlJson) {
			Object obj = new JSONParser(sqlJson).parse();
			if (obj instanceof JSONObject) {
			    JSONObject jsonObject = (JSONObject) obj;
			    return jsonObject;
			} else if(obj instanceof JSONArray){
			    JSONArray jsonArray = (JSONArray) obj;
			    return jsonArray;
			}else {
				try {
					//log.info("getNullableResult(ResultSet rs, int columnIndex)=>JSONObject.parseObject");
					return JSONObject.parseObject(sqlJson);
				}catch (Exception e) {
					try {
						//log.info("getNullableResult(ResultSet rs, int columnIndex)=>JSONArray.parseObject");
						return JSONArray.parseObject(sqlJson,List.class);
					} catch (Exception e1) {
						//log.info("getNullableResult(ResultSet rs, int columnIndex)=>return sqlJson");
						return sqlJson;
					}
				}
			}
		}
		return null;
	}

	@Override
	public Object getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
		String sqlJson = cs.getString(columnIndex);
		if (null != sqlJson) {
			Object obj = new JSONParser(sqlJson).parse();
			if (obj instanceof JSONObject) {
			    JSONObject jsonObject = (JSONObject) obj;
			    return jsonObject;
			} else if(obj instanceof JSONArray){
			    JSONArray jsonArray = (JSONArray) obj;
			    return jsonArray;
			}else {
				try {
					//log.info("getNullableResult(CallableStatement cs, int columnIndex)=>JSONObject.parseObject");
					return JSONObject.parseObject(sqlJson);
				}catch (Exception e) {
					try {
						//log.info("getNullableResult(CallableStatement cs, int columnIndex)=>JSONArray.parseObject");
						return JSONArray.parseObject(sqlJson,List.class);
					} catch (Exception e1) {
						//log.info("getNullableResult(CallableStatement cs, int columnIndex)=>return sqlJson");
						return sqlJson;
					}
				}
			}
		}
		return null;
	}

}
