# the hash function (sha-256 algorithm) must return the checksum displayed below. If not, the newly simulated data is not the same as the original data.
checksum_simulation_summary <- digest(simulation_summary, "sha256")
if(checksum_simulation_summary != "f7211d17408d8cd5ae8d0117530681bd3e3e93d370c96aa1b1759ebda838109a"){
  warning("\u2716 Mismatch between current and original data. Current checksum is: '", checksum_simulation_roundwise, "'")
} else{cat(green("\u2713 Data validated. Current data matches the original data."))
}

checksum_simulation_roundwise <- digest(simulation_roundwise, "sha256")
if(checksum_simulation_roundwise != "953f04444507768b957e4f3ef93aa11649b6b1fde6734cc17dfa240bd4a2c1ca"){
  warning("\u2716 Mismatch between current and original data. Current checksum is: '", checksum_simulation_roundwise, "'")
} else{cat(green("\u2713 Data validated. Current data matches the original data."))
}