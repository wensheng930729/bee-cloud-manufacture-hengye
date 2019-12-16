import React from 'react'
import PropTypes from 'prop-types'
import request from '../../utils/request'
import { Select } from 'antd'
import api from './api'
const Option = Select.Option
const initDistrict = [{ id: -1, district: '请选择' }]
//地区选择组件  待完善
class DistrictSelect extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      provinceData: initDistrict,
      cityData: initDistrict,
      districtDate: initDistrict,
      provinceId: -1,
      cityId: -1,
      districtId: -1
    }
  }

  componentDidMount() {
    const { writeBack } = this.props
    const self = this
    if (writeBack) {
      self.districtChanged().then(() => {
        if (writeBack.province && writeBack.province) {
          self.districtChanged(0, writeBack.province).then(() => {
            if (writeBack.city) {
              self.districtChanged(1, writeBack.city).then(() => {
                if (writeBack.country) {
                  self.districtChanged(2, writeBack.country)
                }
              });
            }
          })
        } else {
          self.districtChanged()
        }
      })
    } else {
      this.districtChanged()
    } 
  }

  //地址选择时
  districtChanged(type = 0, id = 1) {
    const { level, onSelect, mode } = this.props
    const self = this
    if (id === -1) {
      if (level === 1 && onSelect) {
        onSelect('')
      }
      mode === 'strict' && onSelect('')
      this.districtInit(type)
      return new Promise(resolve => {
        return false
      })
    } else if (id === 1 && this.state.provinceData !== initDistrict) {
      //非第一次调用
      return new Promise(resolve => {
        return false
      })
    } else if (type === 0 && level === 1 && onSelect && this.state.provinceData !== initDistrict) {
      self.setState({
        provinceId: id,
        districtValidate: 1
      })
      onSelect(id)
      return new Promise(resolve => {
        return false
      })
    } else if (type === 1 && level === 2 && onSelect) {
      self.setState({
        cityId: id,
        districtValidate: 1
      })
      onSelect(id)
      return new Promise(resolve => {
        return false
      })
    } else if (type === 2 && onSelect) {
      self.setState({
        districtId: id,
        districtValidate: 1
      })
      //属性回调
      onSelect(id)
      return new Promise(resolve => {
        return false
      })
    } else {
      return request(
        api.findRegionByParentId.api(id),
        { method:api.findRegionByParentId.type }
      ).then(res => {
        if (res.code === 0) {
          switch (type) {
            case 0:
              if (id === 1) {
                self.setState({
                  provinceData: [].concat(initDistrict, res.data),
                  districtValidate: 1
                })
              } else {
                self.setState({
                  cityData: [].concat(initDistrict, res.data),
                  districtDate: initDistrict,
                  provinceId: id,
                  cityId: -1,
                  districtId: -1,
                  districtValidate: 1
                })
                mode === 'normal' && onSelect(id)
              }
              break
            case 1:
              self.setState({
                districtDate: [].concat(initDistrict, res.data),
                cityId: id,
                districtId: -1,
                districtValidate: 1
              })
              mode === 'normal' && onSelect(id)
              break
            default:
              break
          }
          return false
        }
      })
    }
  }

  //地址初始化
  districtInit(type) {
    const { onSelect, mode } = this.props;
    switch (type) {
      case 0:
        this.setState({
          cityData: initDistrict,
          districtDate: initDistrict,
          provinceId: -1,
          cityId: -1,
          districtId: -1,
          districtValidate: 1
        })
        mode === 'normal' && onSelect('')
        break
      case 1:
        this.setState({
          districtDate: initDistrict,
          cityId: -1,
          districtId: -1,
          districtValidate: 1
        })
        mode === 'normal' && onSelect(this.state.provinceId)
        break
      case 2:
        this.setState({
          districtId: -1,
          districtValidate: 1
        })
        mode === 'normal' && onSelect(this.state.cityId)
        break

      default:
        break
    }
  }

  render() {
    const self = this
    const {
      provinceData,
      cityData,
      districtDate,
      provinceId,
      cityId,
      districtId
    } = this.state
    const { } = this.props
    return (
      <div style={{ display: 'inline-block' }}>
        <Select
          value={provinceId}
          style={{ width: 120 }}
          onSelect={this.districtChanged.bind(this, 0)}
        >
          {provinceData.map(province => (
            <Option value={province.id} key={province.id}>
              {province.district}
            </Option>
          ))}
        </Select>
        {this.props.level > 1 && (
          <Select
            style={{ width: 120, marginLeft: '10px' }}
            value={cityId}
            onSelect={this.districtChanged.bind(this, 1)}
          >
            {cityData.map(city => (
              <Option value={city.id} key={city.id}>
                {city.district}
              </Option>
            ))}
          </Select>
        )}
        {this.props.level > 2 && (
          <Select
            value={districtId}
            style={{ width: 120, marginLeft: '10px' }}
            onSelect={this.districtChanged.bind(this, 2)}
          >
            {districtDate.map(district => (
              <Option value={district.id} key={district.id}>
                {district.district}
              </Option>
            ))}
          </Select>
        )}
      </div>
    )
  }
}

export default DistrictSelect

//限定控件传入的属性类型
DistrictSelect.propTypes = {
  //选中时的回调
  onSelect: PropTypes.func,
  level: PropTypes.number,
  //每个地区选择框的样式如果为对象则三个为一样，为数组时分别为三个的样式
  // itemStyles: PropTypes.object || PropTypes.array,
  //返回值的模式
  mode: PropTypes.string,//返回值的模式 normal为每次选择都会返回当前值，strict为选择完所有等级才返回
  // value: PropTypes.objectOf(['districtID'])
  writeBack: PropTypes.object,//回写数据 {province, city, country}
}

//设置默认属性
DistrictSelect.defaultProps = {
  onSelect: () => false,
  itemStyles: { width: '120px' },
  level: 2, //选择区域等级  
  mode: 'strict'
}
